// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -I %t

@com(interface: "10000000-0000-0000-0000-000000000001")
protocol IValue {}

@com(interface: "10000000-0000-0000-0000-000000000002")
protocol IClassValue: IValue, AnyObject {}

func invalidReferences(_ value: any IValue, _ classValue: any IClassValue) {
  weak var weakValue: (any IValue)? = value
  // expected-error@-1 {{'any IValue' is incompatible with 'weak' references}}
  unowned var unownedValue: any IValue = value
  // expected-error@-1 {{'any IValue' is incompatible with 'unowned' references}}
  unowned var optionalValue: (any IValue)? = value
  // expected-error@-1 {{'any IValue' is incompatible with 'unowned' references}}

  // A class bound does not make the interface pointer a Swift object pointer.
  weak var weakClassValue: (any IClassValue)? = classValue
  // expected-error@-1 {{'any IClassValue' is incompatible with 'weak' references}}
  unowned var unownedClassValue: any IClassValue = classValue
  // expected-error@-1 {{'any IClassValue' is incompatible with 'unowned' references}}
  unowned var optionalClassValue: (any IClassValue)? = classValue
  // expected-error@-1 {{'any IClassValue' is incompatible with 'unowned' references}}

  weakValue = nil
  unownedValue = value
  optionalValue = nil
  weakClassValue = nil
  unownedClassValue = classValue
  optionalClassValue = nil
  _ = (weakValue, unownedValue, optionalValue,
       weakClassValue, unownedClassValue, optionalClassValue)

  _ = { [weak value] in
    // expected-error@-1 {{'any IValue' is incompatible with 'weak' references}}
    value = nil
    return value
  }
  _ = { [unowned value] in value }
  // expected-error@-1 {{'any IValue' is incompatible with 'unowned' references}}
  _ = { [weak classValue] in
    // expected-error@-1 {{'any IClassValue' is incompatible with 'weak' references}}
    classValue = nil
    return classValue
  }
  _ = { [unowned classValue] in classValue }
  // expected-error@-1 {{'any IClassValue' is incompatible with 'unowned' references}}
}

// Native object references still use Swift reference storage, even when the
// class implements a COM interface.
@com(implementation: "10000000-0000-0000-0000-000000000003")
final class NativeValue: IValue {}

final class NativeHolder {
  weak var weakValue: NativeValue?
  unowned var unownedValue: NativeValue
  unowned(unsafe) var unmanagedValue: NativeValue

  init(_ value: NativeValue) {
    weakValue = value
    unownedValue = value
    unmanagedValue = value
  }
}

final class InterfaceHolder {
  unowned(unsafe) var value: any IValue
  unowned(unsafe) var optional: (any IValue)?
  unowned(unsafe) var classValue: any IClassValue
  unowned(unsafe) var optionalClassValue: (any IClassValue)?

  init(_ value: any IValue, _ classValue: any IClassValue) {
    self.value = value
    self.optional = value
    self.classValue = classValue
    self.optionalClassValue = classValue
  }
}

// A COM-constrained generic with no superclass can hold an interface pointer,
// even when it is class-bound.
func genericReferences<T: IClassValue, U: IValue>(_ value: T, _ other: U) {
  weak var weakValue: T? = value
  // expected-error@-1 {{'T' is incompatible with 'weak' references}}
  unowned var unownedValue: T = value
  // expected-error@-1 {{'T' is incompatible with 'unowned' references}}
  unowned(unsafe) var unmanagedValue: T = value
  // expected-error@-1 {{'T' is incompatible with 'unowned(unsafe)' references}}
  weak var weakOther: U? = other
  // expected-error@-1 {{'U' is incompatible with 'weak' references}}

  weakValue = nil
  unownedValue = value
  unmanagedValue = value
  weakOther = nil
  _ = (weakValue, unownedValue, unmanagedValue, weakOther)

  _ = { [weak value] in
    // expected-error@-1 {{'T' is incompatible with 'weak' references}}
    value = nil
    return value
  }
  _ = { [unowned value] in value }
  // expected-error@-1 {{'T' is incompatible with 'unowned' references}}
}

final class GenericHolder<T: IClassValue> {
  weak var weakValue: T?
  // expected-error@-1 {{'T' is incompatible with 'weak' references}}
  unowned var unownedValue: T
  // expected-error@-1 {{'T' is incompatible with 'unowned' references}}
  unowned(unsafe) var unmanagedValue: T
  // expected-error@-1 {{'T' is incompatible with 'unowned(unsafe)' references}}

  init(_ value: T) {
    weakValue = value
    unownedValue = value
    unmanagedValue = value
  }
}

// A superclass makes the value a Swift object reference.
class Base {}

func superclassReferences<T: Base & IClassValue>(_ value: T) {
  weak var weakValue: T? = value
  unowned var unownedValue: T = value
  unowned(unsafe) var unmanagedValue: T = value
  weakValue = nil
  unownedValue = value
  unmanagedValue = value
  _ = (weakValue, unownedValue, unmanagedValue)
}
