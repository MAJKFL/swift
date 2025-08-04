//===--- Cloner.swift ------------------------------------------------------==//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import OptimizerBridging
import SIL
import SILBridging

/// Clones the initializer value of a GlobalVariable.
///
/// Used to transitively clone "constant" instructions, including their operands,
/// from or to the static initializer value of a GlobalVariable.
///
struct Cloner<Context: MutatingContext> {
  private var bridged: BridgedCloner
  let context: Context

  enum Target {
    case function(Function)
    case global(GlobalVariable)
  }
  let target: Target

  init(cloneToGlobal: GlobalVariable, _ context: Context) {
    self.bridged = BridgedCloner(cloneToGlobal.bridged, context.bridgedPassContext)
    self.context = context
    self.target = .global(cloneToGlobal)
  }

  init(cloneBefore inst: Instruction, _ context: Context) {
    self.bridged = BridgedCloner(inst.bridged, context.bridgedPassContext)
    self.context = context
    self.target = .function(inst.parentFunction)
  }

  init(cloneToEmptyFunction: Function, _ context: Context) where Context == FunctionPassContext {
    self.bridged = BridgedCloner(cloneToEmptyFunction.bridged, context.bridgedPassContext)
    self.context = context
    self.target = .function(cloneToEmptyFunction)
  }

  mutating func deinitialize() {
    bridged.destroy(context.bridgedPassContext)
  }

  var targetFunction: Function {
    guard case .function(let function) = target else {
      fatalError("expected cloning into a function")
    }
    return function
  }

  mutating func clone(instruction: Instruction) -> Instruction {
    let cloned = bridged.clone(instruction.bridged).instruction
    if case .function = target {
      context.notifyInstructionChanged(cloned)
      context.notifyInstructionsChanged()
    }
    return cloned
  }

  /// Transitively clones `value` including its defining instruction's operands.
  mutating func cloneRecursively(value: Value) -> Value {
    if isCloned(value: value) {
      return getClonedValue(of: value)
    }

    guard let inst = value.definingInstruction else {
      fatalError("expected instruction to clone or already cloned value")
    }

    for op in inst.operands {
      _ = cloneRecursively(value: op.value)
    }

    let cloned = clone(instruction: inst)
    if let svi = cloned as? SingleValueInstruction {
      return svi
    } else if let originalMvi = value as? MultipleValueInstructionResult {
      return cloned.results[originalMvi.index]
    }
    fatalError("unexpected instruction kind")
  }

  mutating func cloneUseDefChain(addr: Value, checkBase: (Value) -> Bool) -> Value? {
    // MARK: Hacky temp fix
    if addr is AllocStackInst {
      return nil
    }

    guard !checkBase(addr) else {
      guard let inst = addr as? Instruction else {
        // TODO: Might have to additionally register like the instruction below.
        return addr
      }

      bridged.recordClonedInstruction(inst.bridged, inst.bridged)
      return addr
    }

    switch addr {
    // The cloner does not currently know how to create compensating
    // end_borrows or fix mark_dependence operands.
    case is BeginBorrowInst, is MarkDependenceInst: return nil
    case let singleValueInstruction as SingleValueInstruction:
      // TODO: Double check whether correct
      guard let sourceOperand = singleValueInstruction.operands.first else { return nil }

      return cloneProjection(projectAddr: singleValueInstruction, sourceOperand: sourceOperand, checkBase: checkBase)
    default: return nil
    }
  }

  private func shouldClone(_ value: Value) -> Bool {
    switch value {
    case is StructElementAddrInst, is TupleElementAddrInst, is IndexAddrInst, is TailAddrInst, is InitEnumDataAddrInst, is OpenExistentialAddrInst, is UncheckedTakeEnumDataAddrInst, is ProjectBoxInst, is ProjectBlockStorageInst, is MoveOnlyWrapperToCopyableAddrInst, is CopyableToMoveOnlyWrapperAddrInst, is MoveOnlyWrapperToCopyableBoxInst, is UncheckedAddrCastInst, is AddressToPointerInst, is PointerToAddressInst, is MarkUninitializedInst, is MarkUnresolvedReferenceBindingInst, is DropDeinitInst, is MarkUnresolvedReferenceBindingInst, is MarkDependenceInst, is CopyValueInst, is BeginBorrowInst, is StoreBorrowInst: return true
    default: return false
    }
  }

  private mutating func cloneProjection(
    projectAddr: SingleValueInstruction,
    sourceOperand: Operand,
    checkBase: (Value) -> Bool
  ) -> Value? {
    guard let projectedSource = cloneUseDefChain(
      addr: sourceOperand.value,
      checkBase: checkBase
    ) else {
      return nil
    }

//    for op in projectAddr.operands {
//      _ = cloneUseDefChain(addr: op.value, checkBase: checkBase)
//    }
    
    let clone = clone(instruction: projectAddr)
    clone.setOperand(at: sourceOperand.index, to: projectedSource, context)
    return clone as! SingleValueInstruction
  }

  mutating func getClonedValue(of originalValue: Value) -> Value {
    bridged.getClonedValue(originalValue.bridged).value
  }

  func isCloned(value: Value) -> Bool {
    bridged.isValueCloned(value.bridged)
  }

  func getClonedBlock(for originalBlock: BasicBlock) -> BasicBlock {
    bridged.getClonedBasicBlock(originalBlock.bridged).block
  }

}

extension Cloner where Context == FunctionPassContext {
  func getOrCreateEntryBlock() -> BasicBlock {
    if let entryBlock = targetFunction.blocks.first {
      return entryBlock
    }
    return targetFunction.appendNewBlock(context)
  }

  func cloneFunctionBody(from originalFunction: Function, entryBlockArguments: [Value]) {
    entryBlockArguments.withBridgedValues { bridgedEntryBlockArgs in
      let entryBlock = getOrCreateEntryBlock()
      bridged.cloneFunctionBody(originalFunction.bridged, entryBlock.bridged, bridgedEntryBlockArgs)
    }
  }

  func cloneFunctionBody(from originalFunction: Function) {
    bridged.cloneFunctionBody(originalFunction.bridged)
  }
}

func cloneFunction(from originalFunction: Function, toEmpty targetFunction: Function, _ context: FunctionPassContext) {
  var cloner = Cloner(cloneToEmptyFunction: targetFunction, context)
  defer { cloner.deinitialize() }
  cloner.cloneFunctionBody(from: originalFunction)
}
