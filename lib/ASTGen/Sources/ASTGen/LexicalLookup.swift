//===--- CompilerBuildConfiguration.swift ---------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import ASTBridging
import BasicBridging
import SwiftSyntax
@_spi(Experimental) import SwiftLexicalLookup

private let rowCharWidth: Int = 30

@_cdecl("swift_ASTGen_validateUnqualifiedLookup")
public func unqualifiedLookup(
  sourceFilePtr: UnsafeRawPointer,
  lookupAt: BridgedSourceLoc,
  finishInSequentialScope: Bool,
  astScopeResultRef: BridgedArrayRef
) -> Bool {
  // Obtain source file and lookup position
  let sourceFile = sourceFilePtr.assumingMemoryBound(to: ExportedSourceFile.self)
  let sourceFileSyntax = sourceFile.pointee.syntax
  let sourceLocationConverter = sourceFile.pointee.sourceLocationConverter
  
  guard let lookupPosition = sourceFile.pointee.position(of: lookupAt),
        let lookupToken = sourceFileSyntax.token(at: lookupPosition)
  else {
    print("Could not determine lookup position")
    return false
  }
  
  // Map AST result
  let astResults = astConsumedResults(sourceFile: sourceFile, astScopeResultRef: astScopeResultRef)
  
  // Map SLL result
  let sllResults = sllConsumedResults(lookupToken: lookupToken, finishInSequentialScope: finishInSequentialScope)
  
  // Add header to the output
  var consoleOutput = "-----> Lookup started at: \(sourceLocationConverter.location(for: lookupPosition).lineWithColumn) (\"\(lookupToken.text)\") finishInSequentialScope: \(finishInSequentialScope)\n"
  consoleOutput += "     |" + "ASTScope".addPaddingUpTo(characters: rowCharWidth) + "|" + "SwiftLexicalLookup".addPaddingUpTo(characters: rowCharWidth) + "\n"
  
  // Flagging pass
  flaggingPass(
    astResults: astResults,
    sllResults: sllResults,
    sourceFileSyntax: sourceFileSyntax,
    lookupPosition: lookupPosition
  )
  
  // Matching pass
  let passed = matchingPass(
    astResults: astResults,
    sllResults: sllResults,
    sourceLocationConverter: sourceLocationConverter,
    consoleOutput: &consoleOutput
  )
  
  // Output
//  if !passed {
//    print(consoleOutput)
//  }
  
  print(consoleOutput)
  return passed
}

/// Check if the name at `namePosition`, was improperly introduced
/// by ASTScope (in the same declaration as lookup).
private func isInvalidFirstNameInDeclarationIntroduction(
  sourceFile: SourceFileSyntax,
  lookupPosition: AbsolutePosition,
  namePosition: AbsolutePosition
) -> Bool {
  func firstAncestorOfKind(
    of syntax: SyntaxProtocol?,
    kinds: [SyntaxProtocol.Type]
  ) -> SyntaxProtocol? {
    guard let syntax else { return nil }
    
    for kind in kinds {
      if syntax.is(kind) {
        return syntax
      }
    }
    
    return firstAncestorOfKind(of: syntax.parent, kinds: kinds)
  }
  
  let originToken = sourceFile.token(at: lookupPosition)
  let firstNameToken = sourceFile.token(at: namePosition)
  
  let commonAncestors: [SyntaxProtocol.Type] = [
    SwitchCaseSyntax.self,
    ClosureExprSyntax.self,
    AccessorDeclSyntax.self,
    PatternBindingSyntax.self
  ]
  
  let originAncestor = firstAncestorOfKind(
    of: originToken,
    kinds: commonAncestors
  )
  
  let firstNameAncestor = firstAncestorOfKind(
    of: firstNameToken,
    kinds: commonAncestors
  )
  
  guard let originAncestor,
        let firstNameAncestor,
        originAncestor.kind == firstNameAncestor.kind
  else { return false }
  
  return originAncestor.kind == .patternBinding && originAncestor.id == firstNameAncestor.id
}

/// Returns consumed `ASTScope` results from the
/// given `astScopeResultRef`. Introduces appropriate flags.
private func astConsumedResults(
  sourceFile: UnsafePointer<ExportedSourceFile>,
  astScopeResultRef: BridgedArrayRef
) -> [ConsumedLookupResult] {
  let pointer = astScopeResultRef.data?.assumingMemoryBound(to: BridgedConsumedLookupResult.self)
  let count = astScopeResultRef.count

  let astScopeResultArray = Array(UnsafeBufferPointer(start: pointer, count: count))
  
  return astScopeResultArray.compactMap { bridgedResult in
    let identifierPointer = bridgedResult.name.raw?.assumingMemoryBound(to: CChar.self)
    
    guard let astResultPosition = sourceFile.pointee.position(of: bridgedResult.nameLoc) else {
      print("One of the results, doesn't have a position")
      return nil
    }
    
    return ConsumedLookupResult(
      rawName: identifierPointer == nil ? "" : String(cString: identifierPointer!),
      position: astResultPosition,
      flags: ConsumedLookupResultFlag(rawValue: bridgedResult.flag)
    )
  }
}

/// Performs and returns `SwiftLexicalLookup` lookup and returns
/// the results an array of `ConsumedLookupResult`. Introduces appropriate flags.
private func sllConsumedResults(
  lookupToken: TokenSyntax,
  finishInSequentialScope: Bool
) -> [ConsumedLookupResult] {
  lookupToken.lookup(nil, with: LookupConfig(finishInSequentialScope: finishInSequentialScope))
    .flatMap { result in
      switch result {
      case .lookInMembers(let lookInMembers):
        return [ConsumedLookupResult(
          rawName: "",
          position: lookInMembers.lookupMembersPosition,
          flags: .shouldLookInMembers
        )]
      case .lookInGenericParametersOfExtendedType(let extensionDecl):
        return [ConsumedLookupResult(
          rawName: "",
          position: extensionDecl.extensionKeyword.positionAfterSkippingLeadingTrivia,
          flags: .ignoreNextFromHere
        )]
      case .mightIntroduceDollarIdentifiers(let closure):
        return [ConsumedLookupResult(
          rawName: "",
          position: closure.positionAfterSkippingLeadingTrivia,
          flags: .ignoreNextFromHere
        )]
      default:
        if let parent = result.scope.parent, result.scope.is(GenericParameterClauseSyntax.self) {
          if (parent.is(FunctionDeclSyntax.self) ||
            parent.is(SubscriptDeclSyntax.self) ||
              result.scope.range.contains(lookupToken.position)) {
            // If a result from function generic parameter clause or lookup started within it, reverse introduced names.
            return result.names.reversed().map { name in
              ConsumedLookupResult(rawName: name.identifier?.name ?? "", position: name.position, flags: .placementRearranged)
            }
          } else if let nominalTypeScope = Syntax(parent).asProtocol(SyntaxProtocol.self) as? NominalTypeDeclSyntax, nominalTypeScope.inheritanceClause?.range.contains(lookupToken.position) ?? false {
            // If lookup started from nominal type inheritance clause, reverse introduced names.
            return result.names.reversed().map { name in
              ConsumedLookupResult(rawName: name.identifier?.name ?? "", position: name.position, flags: .placementRearranged)
            }
          }
          
          // No flags or reorderings to perform.
          return result.names.map { name in
            ConsumedLookupResult(rawName: name.identifier?.name ?? "", position: name.position, flags: [])
          }
        } else {
          return result.names.map { name in
            let shouldBeOmitted = name.identifier?.name == "Self" ? !result.scope.is(ProtocolDeclSyntax.self) : false
            
            return ConsumedLookupResult(
              rawName: name.identifier?.name ?? "",
              position: name.position,
              flags: shouldBeOmitted ? [.shouldBeOmitted] : []
            )
          }
        }
      }
    }
}

/// Adds all appropriate flags to `astResults` and `sllResults`.
private func flaggingPass(
  astResults: [ConsumedLookupResult],
  sllResults: [ConsumedLookupResult],
  sourceFileSyntax: SourceFileSyntax,
  lookupPosition: AbsolutePosition
) {
  var i = 0
  var astOffset = 0
  var sllOffset = 0
  var encounteredASTNames = Set<ConsumedLookupResult>()
  var ignoreAt: AbsolutePosition?
  var wasLookupStopped = false
  
  while i < max(astResults.count, sllResults.count) {
    var astResult: ConsumedLookupResult?
    
    if astOffset + i < astResults.count {
      astResult = astResults[astOffset + i]
      
      // Here only to not have to perform force unwraps later.
      guard let astResult else { break }
      
      // Check if lookup was stopped earlier. If so, flag this result with lookupStopped.
      if wasLookupStopped {
        astResult.flags.insert(.lookupStopped)
      }
      
      // Check if this is the end of ast lookup. If so, set wasLookupStopped to true.
      if astResult.isTheEndOfLookup {
        wasLookupStopped = true
      }
      
      // Check if this is not the first encounter of this ast name. If so, should be omitted.
      if !astResult.shouldLookInMembers {
        let isFirstEncounter = !encounteredASTNames.contains(astResult)
        
        if !isFirstEncounter {
          astResult.flags.insert(.shouldBeOmitted)
        }
      }
      
      // Check if names are being currently ignored from at this position. If so, should be omitted.
      if astResult.position == ignoreAt {
        astResult.flags.insert(.shouldBeOmitted)
      }
      
      // Check if this is an invalid introduction within the same declaration. If so, should be omitted.
      if isInvalidFirstNameInDeclarationIntroduction(
        sourceFile: sourceFileSyntax,
        lookupPosition: lookupPosition,
        namePosition: astResult.position
      ) && astResult.name != "self" {
        astResult.flags.insert(.shouldBeOmitted)
      }
      
      // Check if this name should be omitted. If so, continue the loop and add one to offset.
      if astResult.shouldBeOmitted {
        astOffset += 1
        continue
      }
    }
    
    if i + sllOffset < sllResults.count {
      let sllResult = sllResults[i + sllOffset]
      
      // Check if lookup was stopped earlier. If so, flag this result with lookupStopped.
      if wasLookupStopped {
        sllResult.flags.insert(.lookupStopped)
      }
      
      // Check if next results at this position should be ignored. If so, set ignoreAt and omit this name.
      if sllResult.ignoreNextFromHere && sllResult.position != ignoreAt {
        ignoreAt = sllResult.position
        sllResult.flags.insert(.shouldBeOmitted)
      }
      
      // Check if this name should be omitted. If so, continue the loop and add one to offset.
      if sllResult.shouldBeOmitted {
        sllOffset += 1
        continue
      }
    }
    
    if let astResult {
      encounteredASTNames.insert(astResult)
    }
    
    i += 1
  }
}

/// Tries to match both results taking into account previously set
/// flags. Returns whether the test validation succeeded.
private func matchingPass(
  astResults: [ConsumedLookupResult],
  sllResults: [ConsumedLookupResult],
  sourceLocationConverter: SourceLocationConverter,
  consoleOutput: inout String
) -> Bool {
  var i = 0
  var astOffset = 0
  var sllOffset = 0
  var passed = true
  
  while i < max(astResults.count, sllResults.count) {
    var prefix = ""
    var astResultStr = ""
    var sllResultStr = ""
    
    var astResult: ConsumedLookupResult?
    
    if astOffset + i < astResults.count {
      astResult = astResults[astOffset + i]
      
      guard let astResult else { break }
      
      if astResult.shouldBeOmitted {
        consoleOutput += "> ℹ️ | Omitted ASTScope name: \(astResult.consoleLogStr(sourceLocationConverter: sourceLocationConverter))\n"
        astOffset += 1
        continue
      }
      
      astResultStr += astResult.consoleLogStr(sourceLocationConverter: sourceLocationConverter)
    } else {
      astResultStr = "-----"
    }
    
    var sllResult: ConsumedLookupResult?
    
    if i + sllOffset < sllResults.count {
      sllResult = sllResults[i + sllOffset]
      
      guard let sllResult else { break }
      
      if sllResult.shouldBeOmitted {
        consoleOutput += "> ℹ️ | Omitted SwiftLexicalLookup name: \(sllResult.consoleLogStr(sourceLocationConverter: sourceLocationConverter))\n"
        sllOffset += 1
        continue
      }
      
      sllResultStr = sllResult.consoleLogStr(sourceLocationConverter: sourceLocationConverter)
    } else {
      sllResultStr = "-----"
    }
    
    i += 1
    
    guard astResult != nil || sllResult != nil else { continue }
    
    if let astResult, let sllResult {
      if (astResult.position == sllResult.position &&
          astResult.name == sllResult.name) {
        prefix = "✅"
      } else if astResult.lookupStopped || sllResult.lookupStopped {
        prefix = "⏩"
      } else if astResult.position == sllResult.position ||
                astResult.name == sllResult.name {
        prefix = "⚠️"
        passed = false
      } else {
        prefix = "❌"
        passed = false
      }
    } else if (astResult?.lookupStopped ?? false) || (sllResult?.lookupStopped ?? false) {
      prefix = "⏩"
    } else {
      prefix = "❌"
      passed = false
    }
    
    consoleOutput += "> \(prefix) |\(astResultStr.addPaddingUpTo(characters: rowCharWidth))|\(sllResultStr.addPaddingUpTo(characters: rowCharWidth))"
    
    consoleOutput += "\n"
  }
  
  return passed
}

/// Simple representation of lookup result.
/// Contains flags that indicate additional behaviour.
private class ConsumedLookupResult: Hashable {
  var rawName: String
  var position: AbsolutePosition
  var flags: ConsumedLookupResultFlag
  
  init(
    rawName: String,
    position: AbsolutePosition,
    flags: ConsumedLookupResultFlag
  ) {
    self.rawName = rawName
    self.position = position
    self.flags = flags
  }
  
  var name: String {
    shouldLookInMembers ? "" : rawName
  }
  
  var isTheEndOfLookup: Bool {
    flags.contains(.endOfLookup)
  }
  
  var shouldLookInMembers: Bool {
    flags.contains(.shouldLookInMembers)
  }
  
  var resultPlacementRearranged: Bool {
    flags.contains(.placementRearranged)
  }
  
  var shouldBeOmitted: Bool {
    flags.contains(.shouldBeOmitted)
  }
  
  var ignoreNextFromHere: Bool {
    flags.contains(.ignoreNextFromHere)
  }
  
  var lookupStopped: Bool {
    flags.contains(.lookupStopped)
  }
  
  func consoleLogStr(sourceLocationConverter: SourceLocationConverter) -> String {
    (isTheEndOfLookup ? "End here: " : "") +
    (resultPlacementRearranged ? "↕️ " : "") +
    (ignoreNextFromHere ? "Ignore next from: " : "") +
    (shouldLookInMembers ? "Look memb: " : "\(name) ") +
    sourceLocationConverter.location(for: position).lineWithColumn
  }
  
  static func ==(lhs: ConsumedLookupResult, rhs: ConsumedLookupResult) -> Bool {
    return lhs.rawName == rhs.rawName &&
    lhs.position == rhs.position &&
    lhs.flags == rhs.flags
  }
  
  func hash(into hasher: inout Hasher) {
    hasher.combine(rawName)
    hasher.combine(position)
    hasher.combine(flags)
  }
}

/// Determine behaviour during matching pass.
struct ConsumedLookupResultFlag: OptionSet, Hashable {
  let rawValue: Int

  static let endOfLookup = ConsumedLookupResultFlag(rawValue: 1 << 0)
  static let shouldLookInMembers = ConsumedLookupResultFlag(rawValue: 1 << 1)
  static let placementRearranged = ConsumedLookupResultFlag(rawValue: 1 << 2)
  static let shouldBeOmitted = ConsumedLookupResultFlag(rawValue: 1 << 3)
  static let lookupStopped = ConsumedLookupResultFlag(rawValue: 1 << 4)
  static let ignoreNextFromHere = ConsumedLookupResultFlag(rawValue: 1 << 5)
}

extension SourceLocation {
  fileprivate var lineWithColumn: String {
    return "\(line):\(column)"
  }
}

extension String {
  fileprivate func addPaddingUpTo(characters charCount: Int) -> String {
    guard self.count < charCount else { return self }
    
    let lengthDifference = charCount - self.count
    
    var leftPad = ""
    var rightPad = ""
    
    for _ in 0..<Int(floor(Double(lengthDifference) / 2.0)) {
      leftPad += " "
    }
    
    for _ in 0..<Int(ceil(Double(lengthDifference) / 2.0)) {
      rightPad += " "
    }
    
    return leftPad + self + rightPad
  }
}
