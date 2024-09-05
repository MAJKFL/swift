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
  
  let lookupResults = performLookupAt.lookup(nil, with: LookupConfig(propagateToParent: propagateToParent)).flatMap { result in
    result.names
  }
  
  var consoleOutput = "-----> Lookup started at: \(sourceLocationConverter.location(for: lookupPosition).lineWithColumn)\n"
  consoleOutput += "     |" + "ASTScope".addPaddingUpTo(characters: 20) + "|" + "SwiftLexicalLookup".addPaddingUpTo(characters: 20) + "\n"
  
  let pointer = astScopeResultRef.data?.assumingMemoryBound(to: BridgedConsumedLookupResult.self)
  let count = astScopeResultRef.count

  let astScopeResultArray = Array(UnsafeBufferPointer(start: pointer, count: count))
  
  var astResultOffset = 0
  var passed = true
  var wasLookupStopped = false
  
  if let firstASTResult = astScopeResultArray.first {
    let resultPosition = sourceFile.pointee.position(of: firstASTResult.nameLoc)!
    let token = sourceFileSyntax.token(at: resultPosition)
    
    // Check if the first name from declaration was introduced before it's end. COULD POSSIBLY OMIT BUGS!!!
    if isInvalidFirstNameInDeclarationIntroduction(
      sourceFile: sourceFileSyntax,
      lookupPosition: lookupPosition,
      firstNamePosition: resultPosition
    ) {
      consoleOutput += "> ℹ️ | Omitted first ASTScope name: \(token!.text) \(sourceLocationConverter.location(for: resultPosition).lineWithColumn)\n"
      astResultOffset = 1
    }
  }
  
  for i in 0..<max(astScopeResultArray.count, lookupResults.count) {
    var prefix = ""
    var astResultStr = ""
    var newResultStr = ""
    
    var astResultPosition: AbsolutePosition?
    var astResultIdentifierStr: String?
    var flag = 0
    
    if astResultOffset + i < astScopeResultArray.count {
      let consumedLookupResult = astScopeResultArray[astResultOffset + i]
      
      let identifierPointer = consumedLookupResult.name.raw!.assumingMemoryBound(to: CChar.self)
      astResultPosition = sourceFile.pointee.position(of: consumedLookupResult.nameLoc)!
      
      flag = consumedLookupResult.flag
      if flag == 1 {
        wasLookupStopped = true
      }
      
      astResultIdentifierStr = String(cString: identifierPointer)
      
      astResultStr = astResultIdentifierStr! + " " + sourceLocationConverter.location(for: astResultPosition!).lineWithColumn
    } else if i >= astScopeResultArray.count {
      if !wasLookupStopped {
        prefix = "❌"
        passed = false
      }
      astResultStr = "-----"
    }
    
    var newResultPosition: AbsolutePosition?
    var newResultIdentifierStr: String?
    
    if i < lookupResults.count {
      let newResult = lookupResults[i]
      
      newResultPosition = newResult.syntax.position
      newResultIdentifierStr = newResult.identifier!.name
      
      newResultStr = newResultIdentifierStr! + " " + sourceLocationConverter.location(for: newResultPosition!).lineWithColumn
    } else if i < astScopeResultArray.count - astResultOffset {
      if !wasLookupStopped {
        prefix = "❌"
        passed = false
      }
      
      newResultStr = "-----"
    }
    
    if let astResultPosition,
       let astResultIdentifierStr,
       let newResultPosition,
        let newResultIdentifierStr {
      if (astResultPosition == newResultPosition &&
          astResultIdentifierStr == newResultIdentifierStr || wasLookupStopped) {
        prefix = "✅"
      } else if astResultPosition == newResultPosition ||
                astResultIdentifierStr == newResultIdentifierStr {
        prefix = "⚠️"
        passed = false
      } else {
        prefix = "❌"
        passed = false
      }
    } else if wasLookupStopped {
      prefix = "✅"
    }
    
    guard astResultPosition != nil || newResultPosition != nil else { continue }
    
    consoleOutput += "> \(prefix) |\(astResultStr.addPaddingUpTo(characters: 20))|\(newResultStr.addPaddingUpTo(characters: 20))"
    
    if flag == 1 {
      consoleOutput += "-> Lookup stop flag"
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
  
  let originClosestVariableDeclAncestor = originToken?.ancestorOrSelf { syntax in
    syntax.as(VariableDeclSyntax.self)
  }
  let firstNameClosestVariableDeclAncestor = firstNameToken?.ancestorOrSelf { syntax in
    syntax.as(VariableDeclSyntax.self)
  }
  
  guard let originClosestVariableDeclAncestor,
        let firstNameClosestVariableDeclAncestor
  else { return false }
  
  return originClosestVariableDeclAncestor == firstNameClosestVariableDeclAncestor
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
