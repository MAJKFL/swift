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
  
  let lookupResults = performLookupAt.lookup(nil).flatMap { result in
    result.names
  }
  
  let pointer = astScopeResultRef.data?.assumingMemoryBound(to: BridgedLocatedIdentifier.self)
  let count = astScopeResultRef.count

  let astScopeResultArray = Array(UnsafeBufferPointer(start: pointer, count: count))
  
  var passed = true
  
  for i in 0..<max(astScopeResultArray.count, lookupResults.count) {
    var prefix = ""
    var astResultStr = ""
    var newResultStr = ""
    
    var astResultPosition: AbsolutePosition?
    var astResultIdentifierStr: String?
    
    if i < astScopeResultArray.count {
      let locatedIdentifier = astScopeResultArray[i]
      
      let identifierPointer = locatedIdentifier.name.raw!.assumingMemoryBound(to: CChar.self)
      astResultPosition = sourceFile.pointee.position(of: locatedIdentifier.nameLoc)!
      
      astResultIdentifierStr = String(cString: identifierPointer)
      
      astResultStr = astResultIdentifierStr! + " " + sourceLocationConverter.location(for: astResultPosition!).lineWithColumn
    } else {
      prefix = "❌"
      astResultStr = "-----"
      passed = false
    }
    
    var newResultPosition: AbsolutePosition?
    var newResultIdentifierStr: String?
    
    if i < lookupResults.count {
      let newResult = lookupResults[i]
      
      newResultPosition = newResult.syntax.position
      newResultIdentifierStr = newResult.identifier!.name
      
      newResultStr = newResultIdentifierStr! + " " + sourceLocationConverter.location(for: newResultPosition!).lineWithColumn
    } else {
      prefix = "❌"
      newResultStr = "-----"
      passed = false
    }
    
    if let astResultPosition,
       let astResultIdentifierStr,
       let newResultPosition,
        let newResultIdentifierStr {
      if astResultPosition == newResultPosition &&
          astResultIdentifierStr == newResultIdentifierStr {
        prefix = "✅"
      } else {
        prefix = "❌"
        passed = false
      }
    }
    
    print("> \(prefix) |\(astResultStr.addPaddingUpTo(characters: 20))|\(newResultStr.addPaddingUpTo(characters: 20))")
  }
  
  return passed
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
