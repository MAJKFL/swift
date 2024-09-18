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

@_cdecl("swift_ASTGen_validateUnqualifiedLookup")
public func unqualifiedLookup(
  sourceFilePtr: UnsafeRawPointer,
  lookupAt: BridgedSourceLoc,
  propagateToParent: Bool,
  astScopeResultRef: BridgedArrayRef
) -> Bool {
  let sourceFile = sourceFilePtr.assumingMemoryBound(to: ExportedSourceFile.self)
  let sourceFileSyntax = sourceFile.pointee.syntax
  let sourceLocationConverter = sourceFile.pointee.sourceLocationConverter
  
  guard let lookupPosition = sourceFile.pointee.position(of: lookupAt),
        let performLookupAt = sourceFileSyntax.token(at: lookupPosition)
  else {
    print("Could not determine lookup start position")
    return false
  }
  
  let pointer = astScopeResultRef.data?.assumingMemoryBound(to: BridgedConsumedLookupResult.self)
  let count = astScopeResultRef.count

  let astScopeResultArray = Array(UnsafeBufferPointer(start: pointer, count: count))
  
  let ASTScopeResults = astScopeResultArray.map { bridgedResult in
    let identifierPointer = bridgedResult.name.raw?.assumingMemoryBound(to: CChar.self)
    let astResultPosition = sourceFile.pointee.position(of: bridgedResult.nameLoc)!
    
    return ConsumedLookupResult(
      rawName: identifierPointer == nil ? "" : String(cString: identifierPointer!),
      position: astResultPosition,
      flag: ConsumedLookupResultFlag(rawValue: bridgedResult.flag)
    )
  }
  
  let SLLookupResults = performLookupAt.lookup(nil, with: LookupConfig(finishInSequentialScope: propagateToParent, includeMembers: false))
    .flatMap { result in
      if case .lookInMembers(let lookInMembers) = result {
        return [ConsumedLookupResult(rawName: "", position: lookInMembers.lookupMembersPosition, flag: .shouldLookInMembers)]
      } else {
        if let parent = result.scope.parent,
           result.scope.is(GenericParameterClauseSyntax.self),
           (parent.is(FunctionDeclSyntax.self) ||
            parent.is(SubscriptDeclSyntax.self) ||
            result.scope.range.contains(lookupPosition)) { // If a result from function generic parameter clause or lookup started within it, reverse introduced names. Simple heuristic to deal with weird ASTScope behavior.
          return result.names.reversed().map { name in
            ConsumedLookupResult(rawName: name.identifier?.name ?? "", position: name.position, flag: .placementRearranged)
          }
        } else {
          return result.names.map { name in
            ConsumedLookupResult(rawName: name.identifier?.name ?? "", position: name.position, flag: [])
          }
        }
      }
    }
  
  var consoleOutput = "-----> Lookup started at: \(sourceLocationConverter.location(for: lookupPosition).lineWithColumn) (\"\(performLookupAt.text)\")\n"
  consoleOutput += "     |" + "ASTScope".addPaddingUpTo(characters: 20) + "|" + "SwiftLexicalLookup".addPaddingUpTo(characters: 20) + "\n"
  
  var i = 0
  var astResultOffset = 0
  var encounteredASTNames = Set<ConsumedLookupResult>()
  var passed = true
  var wasLookupStopped = false
  
  while i < max(ASTScopeResults.count, SLLookupResults.count) {
    var prefix = ""
    var astResultStr = ""
    var sllResultStr = ""
    
    var astResult: ConsumedLookupResult?
    
    if astResultOffset + i < ASTScopeResults.count {
      astResult = ASTScopeResults[astResultOffset + i]
      
      if !astResult!.shouldLookInMembers {
        let (isFirstEncounter, _) = encounteredASTNames.insert(astResult!)
        
        guard isFirstEncounter else {
          consoleOutput += "> ℹ️ | Omitted ASTScope name: \(astResult!.consoleLogStr(sourceLocationConverter: sourceLocationConverter))\n"
          astResultOffset += 1
          continue
        }
      }
      
      // Check if the first name from declaration was introduced before it's end. COULD POSSIBLY OMIT BUGS!!!
      if isInvalidFirstNameInDeclarationIntroduction(
        sourceFile: sourceFileSyntax,
        lookupPosition: lookupPosition,
        firstNamePosition: astResult!.position
      ) {
        consoleOutput += "> ℹ️ | Omitted ASTScope name: \(astResult!.consoleLogStr(sourceLocationConverter: sourceLocationConverter))\n"
        astResultOffset += 1
        continue
      }
      
      astResultStr += astResult!.consoleLogStr(sourceLocationConverter: sourceLocationConverter)
    } else if i >= ASTScopeResults.count {
      if !wasLookupStopped {
        prefix = "❌"
        passed = false
      }
      astResultStr = "-----"
    }
    
    var sllResult: ConsumedLookupResult?
    
    if i < SLLookupResults.count {
      sllResult = SLLookupResults[i]
      
      sllResultStr = sllResult!.consoleLogStr(sourceLocationConverter: sourceLocationConverter)
    } else if i - astResultOffset >= SLLookupResults.count {
      if !wasLookupStopped {
        prefix = "❌"
        passed = false
      }
      
      sllResultStr = "-----"
    }
    
    i += 1
    
    guard astResult != nil || sllResult != nil else { continue }
    
    if let astResult, let sllResult {
      if (astResult.position == sllResult.position &&
          astResult.name == sllResult.name || wasLookupStopped) {
        prefix = "✅"
      } else if astResult.position == sllResult.position ||
                astResult.name == sllResult.name {
        prefix = "⚠️"
        passed = false
      } else {
        prefix = "❌"
        passed = false
      }
    } else if wasLookupStopped {
      prefix = "⏩"
    } else {
      prefix = "❌"
      passed = false
    }
    
    consoleOutput += "> \(prefix) |\(astResultStr.addPaddingUpTo(characters: 20))|\(sllResultStr.addPaddingUpTo(characters: 20))"
    
    if let astResult, astResult.isTheEndOfLookup {
      wasLookupStopped = true
    }
    
    consoleOutput += "\n"
  }
  
  if !passed {
    print(consoleOutput)
  }
  
  return passed
}

private func isInvalidFirstNameInDeclarationIntroduction(sourceFile: SourceFileSyntax,
                                                         lookupPosition: AbsolutePosition,
                                                         firstNamePosition: AbsolutePosition) -> Bool {
  let originToken = sourceFile.token(at: lookupPosition)
  let firstNameToken = sourceFile.token(at: firstNamePosition)
  
  // Check if the two positions are in closure expression.
  let originClosestClosureExprAncestor = originToken?.ancestorOrSelf { syntax in
    syntax.as(ClosureExprSyntax.self)
  }
  let firstNameClosestClosureExprAncestor = firstNameToken?.ancestorOrSelf { syntax in
    syntax.as(ClosureExprSyntax.self)
  }
  
  if let originClosestClosureExprAncestor,
     let firstNameClosestClosureExprAncestor,
     originClosestClosureExprAncestor == firstNameClosestClosureExprAncestor {
    return false
  }
  
  // Check if the two positions are in accessor declaration.
  let originClosestAccessorDeclAncestor = originToken?.ancestorOrSelf { syntax in
    syntax.as(AccessorDeclSyntax.self)
  }
  let firstNameClosestAccessorDeclAncestor = firstNameToken?.ancestorOrSelf { syntax in
    syntax.as(AccessorDeclSyntax.self)
  }
  
  if let originClosestAccessorDeclAncestor,
     let firstNameClosestAccessorDeclAncestor,
     originClosestAccessorDeclAncestor == firstNameClosestAccessorDeclAncestor {
    return false
  }
  
  // Check if the two positions are in separate pattern bindings.
  let originClosestPatternBindingAncestor = originToken?.ancestorOrSelf { syntax in
    syntax.as(PatternBindingSyntax.self)
  }
  let firstNameClosestPatternBindingAncestor = firstNameToken?.ancestorOrSelf { syntax in
    syntax.as(PatternBindingSyntax.self)
  }
  
  guard let originClosestPatternBindingAncestor,
        let firstNameClosestPatternBindingAncestor
  else {
    return false
  }
  
  return originClosestPatternBindingAncestor == firstNameClosestPatternBindingAncestor
}

fileprivate struct ConsumedLookupResult: Hashable {
  var rawName: String
  var position: AbsolutePosition
  var flag: ConsumedLookupResultFlag
  
  var name: String {
    shouldLookInMembers ? "" : rawName
  }
  
  var isTheEndOfLookup: Bool {
    flag.contains(.endOfLookup)
  }
  
  var shouldLookInMembers: Bool {
    flag.contains(.shouldLookInMembers)
  }
  
  var resultPlacementRearranged: Bool {
    flag.contains(.placementRearranged)
  }
  
  func consoleLogStr(sourceLocationConverter: SourceLocationConverter) -> String {
    (isTheEndOfLookup ? "End here: " : "") +
    (resultPlacementRearranged ? "↕️ " : "") +
    (shouldLookInMembers ? "Look memb: " : "\(name) ") +
    sourceLocationConverter.location(for: position).lineWithColumn
  }
}

struct ConsumedLookupResultFlag: OptionSet, Hashable {
  let rawValue: Int

  static let endOfLookup = ConsumedLookupResultFlag(rawValue: 1 << 0)
  static let shouldLookInMembers = ConsumedLookupResultFlag(rawValue: 1 << 1)
  static let placementRearranged = ConsumedLookupResultFlag(rawValue: 1 << 2)
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
