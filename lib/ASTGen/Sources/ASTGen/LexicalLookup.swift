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
) {
  let sourceFile = sourceFilePtr.assumingMemoryBound(to: ExportedSourceFile.self)
  let sourceFileSyntax = sourceFile.pointee.syntax
  
  guard let lookupPosition = sourceFile.pointee.position(of: lookupAt),
        let performLookupAt = sourceFileSyntax.token(at: lookupPosition)
  else {
    print("Could not determine lookup start position")
    return
  }
  
  let lookupResults = performLookupAt.lookup(nil)
  
  let pointer = astScopeResultRef.data?.assumingMemoryBound(to: BridgedLocatedIdentifier.self)
  let count = astScopeResultRef.count

  let astScopeResultArray = Array(UnsafeBufferPointer(start: pointer, count: count))

  astScopeResultArray.forEach { locatedIdentifier in
    print(locatedIdentifier)
  }
}
