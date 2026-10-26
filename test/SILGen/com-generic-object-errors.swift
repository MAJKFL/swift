// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -enable-builtin-module -target %target-swift-6.0-abi-triple -I %t -emit-silgen -verify %s
// RUN: %target-swift-frontend -enable-experimental-com-interop -enable-builtin-module -target %target-swift-6.0-abi-triple -enable-sil-opaque-values -I %t -emit-silgen -verify %s

// REQUIRES: concurrency

import Builtin

@com(interface: "51000000-0000-0000-0000-000000000001")
protocol IItem {}
@com(interface: "51000000-0000-0000-0000-000000000002")
protocol IClassItem: IItem, AnyObject {}

func object<T: IClassItem>(_ x: T) -> AnyObject {
  x as AnyObject
  // expected-error@-1 {{cannot convert 'T' to 'AnyObject' because 'T' may hold a COM interface pointer}}
}

func identical<T: IClassItem>(_ x: T, _ y: T) -> Bool {
  x === y
  // expected-error@-1 2 {{cannot convert 'T' to 'AnyObject' because 'T' may hold a COM interface pointer}}
}

func identifier<T: IClassItem>(_ x: T) -> ObjectIdentifier {
  ObjectIdentifier(x)
  // expected-error@-1 {{cannot convert 'T' to 'AnyObject' because 'T' may hold a COM interface pointer}}
}

func retain<T>(_ x: T) {
  Builtin.retain(x)
  // expected-error@-1 {{invalid use of builtin: retain operand must be an object type}}
}

func release<T>(_ x: T) {
  Builtin.release(x)
  // expected-error@-1 {{invalid use of builtin: release operand must be an object type}}
}

func autorelease<T: IClassItem>(_ x: T) {
  Builtin.autorelease(x)
  // expected-error@-1 {{invalid use of builtin: autorelease operand must be an object type}}
}

func toBridgeObject<T: IClassItem>(_ x: T, _ bits: Builtin.Word) -> Builtin.BridgeObject {
  Builtin.castToBridgeObject(x, bits)
  // expected-error@-1 {{invalid use of builtin: castToBridgeObject source must be a class}}
}

func fromBridgeObject<T: IClassItem>(_ x: Builtin.BridgeObject) -> T {
  Builtin.castReferenceFromBridgeObject(x)
  // expected-error@-1 {{invalid use of builtin: castReferenceFromBridgeObject dest must be an object type}}
}

func beginCOWMutation<T>(_ x: inout T) -> Builtin.Int1 {
  Builtin.beginCOWMutation(&x)
  // expected-error@-1 {{invalid use of builtin: beginCOWMutation operand must be an object type}}
}

func beginCOWMutationNative<T: IClassItem>(_ x: inout T) -> Builtin.Int1 {
  Builtin.beginCOWMutation_native(&x)
  // expected-error@-1 {{invalid use of builtin: beginCOWMutation operand must be an object type}}
}

func serial<T: IItem & SerialExecutor>(_ x: T) -> Builtin.Executor {
  Builtin.buildOrdinarySerialExecutorRef(x)
  // expected-error@-1 {{invalid use of builtin: executor operand must be an object type}}
}

func complexEquality<T: IItem & SerialExecutor>(_ x: T) -> Builtin.Executor {
  Builtin.buildComplexEqualitySerialExecutorRef(x)
  // expected-error@-1 {{invalid use of builtin: executor operand must be an object type}}
}

func task<T: IItem & TaskExecutor>(_ x: T) -> Builtin.Executor {
  Builtin.buildOrdinaryTaskExecutorRef(x)
  // expected-error@-1 {{invalid use of builtin: executor operand must be an object type}}
}

func defaultActor<T: IClassItem>(_ x: T) -> Builtin.Executor {
  Builtin.buildDefaultActorExecutorRef(x)
  // expected-error@-1 {{invalid use of builtin: executor operand must be an object type}}
}

// A superclass makes the value a Swift object reference.
class Base {}

func superclassObject<T: Base & IClassItem>(_ x: T) -> AnyObject {
  Builtin.retain(x)
  return x as AnyObject
}
