//===--- Set.swift - sets for basic blocks, values and instructions -------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SILBridging

public protocol IntrusiveSet : CustomStringConvertible, NoReflectionChildren {
  associatedtype Element

  init(_ context: some Context)
  mutating func insert(_ element: Element) -> Bool
  mutating func erase(_ element: Element)
  func contains(_ element: Element) -> Bool
  mutating func deinitialize()
}

/// A set of basic blocks.
///
/// This is an extremely efficient implementation which does not need memory
/// allocations or hash lookups.
///
/// This type should be a move-only type, but unfortunately we don't have move-only
/// types yet. Therefore it's needed to call `deinitialize()` explicitly to
/// destruct this data structure, e.g. in a `defer {}` block.
public struct BasicBlockSet : IntrusiveSet {

  private let context: BridgedContext
  private let bridged: BridgedBasicBlockSet
    
  public init(_ context: some Context) {
    self.context = context._bridged
    self.bridged = self.context.allocBasicBlockSet()
  }

  public func contains(_ block: BasicBlock) -> Bool {
    bridged.contains(block.bridged)
  }

  /// Returns true if `block` was not contained in the set before inserting.
  @discardableResult
  public mutating func insert(_ block: BasicBlock) -> Bool {
    bridged.insert(block.bridged)
  }

  public mutating func erase(_ block: BasicBlock) {
    bridged.erase(block.bridged)
  }

  public var description: String {
    let function = bridged.getFunction().function
    let blockNames = function.blocks.enumerated().filter { contains($0.1) }
                                                 .map { "bb\($0.0)"}
    return "{" + blockNames.joined(separator: ", ") + "}"
  }

  /// TODO: once we have move-only types, make this a real deinit.
  public mutating func deinitialize() {
    context.freeBasicBlockSet(bridged)
  }
}

/// A set of values.
///
/// This is an extremely efficient implementation which does not need memory
/// allocations or hash lookups.
///
/// This type should be a move-only type, but unfortunately we don't have move-only
/// types yet. Therefore it's needed to call `deinitialize()` explicitly to
/// destruct this data structure, e.g. in a `defer {}` block.
public struct ValueSet : IntrusiveSet {

  private let context: BridgedContext
  private let bridged: BridgedNodeSet
    
  public init(_ context: some Context) {
    self.context = context._bridged
    self.bridged = self.context.allocNodeSet()
  }

  public func contains(_ value: Value) -> Bool {
    bridged.containsValue(value.bridged)
  }

  /// Returns true if `value` was not contained in the set before inserting.
  @discardableResult
  public mutating func insert(_ value: Value) -> Bool {
    bridged.insertValue(value.bridged)
  }

  public mutating func erase(_ value: Value) {
    bridged.eraseValue(value.bridged)
  }

  public var description: String {
    let function = bridged.getFunction().function
    var d = "{\n"
    for block in function.blocks {
      for arg in block.arguments {
        if contains(arg) {
          d += arg.description
        }
      }
      for inst in block.instructions {
        for result in inst.results {
          if contains(result) {
            d += result.description
          }
        }
      }
    }
    d += "}\n"
    return d
  }

  /// TODO: once we have move-only types, make this a real deinit.
  public mutating func deinitialize() {
    context.freeNodeSet(bridged)
  }
}

/// A set of instructions.
///
/// This is an extremely efficient implementation which does not need memory
/// allocations or hash lookups.
///
/// This type should be a move-only type, but unfortunately we don't have move-only
/// types yet. Therefore it's needed to call `deinitialize()` explicitly to
/// destruct this data structure, e.g. in a `defer {}` block.
public struct SpecificInstructionSet<InstType: Instruction> : IntrusiveSet {

  private let context: BridgedContext
  private let bridged: BridgedNodeSet
    
  public init(_ context: some Context) {
    self.context = context._bridged
    self.bridged = self.context.allocNodeSet()
  }

  public func contains(_ inst: InstType) -> Bool {
    bridged.containsInstruction(inst.bridged)
  }

  /// Returns true if `inst` was not contained in the set before inserting.
  @discardableResult
  public mutating func insert(_ inst: InstType) -> Bool {
    bridged.insertInstruction(inst.bridged)
  }

  public mutating func erase(_ inst: InstType) {
    bridged.eraseInstruction(inst.bridged)
  }

  public var description: String {
    let function = bridged.getFunction().function
    var d = "{\n"
    for i in function.instructions {
      if let inst = i as? InstType, contains(inst) {
        d += inst.description + "\n"
      }
    }
    d += "}\n"
    return d
  }

  /// TODO: once we have move-only types, make this a real deinit.
  public mutating func deinitialize() {
    context.freeNodeSet(bridged)
  }
}

/// An `InstructionSet` which also provides a `count` property.
public struct SpecificInstructionSetWithCount<InstType: Instruction> : IntrusiveSet {
  public private(set) var count = 0
  private var underlyingSet: SpecificInstructionSet<InstType>

  public init(_ context: some Context) {
    self.underlyingSet = SpecificInstructionSet(context)
  }

  public func contains(_ inst: InstType) -> Bool { underlyingSet.contains(inst) }

  public var isEmpty: Bool { count == 0 }

  /// Returns true if `inst` was not contained in the set before inserting.
  @discardableResult
  public mutating func insert(_ inst: InstType) -> Bool {
    if underlyingSet.insert(inst) {
      count += 1
      return true
    }
    return false
  }

  public mutating func erase(_ inst: InstType) {
    if underlyingSet.contains(inst) {
      count -= 1
      assert(count >= 0)
    }
    underlyingSet.erase(inst)
  }

  public var description: String { underlyingSet.description }

  public mutating func deinitialize() { underlyingSet.deinitialize() }
}

public typealias InstructionSet = SpecificInstructionSet<Instruction>
public typealias InstructionSetWithCount = SpecificInstructionSetWithCount<Instruction>

/// A set of operands.
///
/// This is an extremely efficient implementation which does not need memory
/// allocations or hash lookups.
///
/// This type should be a move-only type, but unfortunately we don't have move-only
/// types yet. Therefore it's needed to call `deinitialize()` explicitly to
/// destruct this data structure, e.g. in a `defer {}` block.
public struct OperandSet : IntrusiveSet {

  private let context: BridgedContext
  private let bridged: BridgedOperandSet

  public init(_ context: some Context) {
    self.context = context._bridged
    self.bridged = self.context.allocOperandSet()
  }

  public func contains(_ operand: Operand) -> Bool {
    bridged.contains(operand.bridged)
  }

  /// Returns true if `inst` was not contained in the set before inserting.
  @discardableResult
  public mutating func insert(_ operand: Operand) -> Bool {
    bridged.insert(operand.bridged)
  }

  public mutating func erase(_ operand: Operand) {
    bridged.erase(operand.bridged)
  }

  public var description: String {
    let function = bridged.getFunction().function
    var d = "{\n"
    for inst in function.instructions {
      for op in inst.operands {
        if contains(op) {
          d += op.description
        }
      }
    }
    d += "}\n"
    return d
  }

  /// TODO: once we have move-only types, make this a real deinit.
  public mutating func deinitialize() {
    context.freeOperandSet(bridged)
  }
}

extension InstructionSet {
  public mutating func insert<I: Instruction>(contentsOf source: some Sequence<I>) {
    for inst in source {
      _ = insert(inst)
    }
  }
}

extension IntrusiveSet {
  public mutating func insert(contentsOf source: some Sequence<Element>) {
    for element in source {
      _ = insert(element)
    }
  }

  public init(insertContentsOf source: some Sequence<Element>, _ context: some Context) {
    self.init(context)
    insert(contentsOf: source)
  }
}
