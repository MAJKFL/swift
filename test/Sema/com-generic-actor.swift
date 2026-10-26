// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -I %t

// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed

@com(interface: "10000000-0000-0000-0000-000000000001")
protocol IValue {}

@com(interface: "10000000-0000-0000-0000-000000000002")
protocol IClassValue: IValue, AnyObject {}

func isolated<T: IValue & Actor>(_ value: isolated T) async {}
// expected-error@-1 {{generic parameter 'T' cannot conform to both COM interface 'IValue' and 'Actor'}}

func distributed<T: IClassValue & DistributedActor>(_ value: T) {}
// expected-error@-1 {{generic parameter 'T' cannot conform to both COM interface 'IClassValue' and 'DistributedActor'}}

struct Holder<T: IValue> {
  func method<U>(_ value: U) where U: IClassValue, U: Actor {}
  // expected-error@-1 {{generic parameter 'U' cannot conform to both COM interface 'IClassValue' and 'Actor'}}
}

extension IValue where Self: Actor {
// expected-error@-1 {{generic parameter 'Self' cannot conform to both COM interface 'IValue' and 'Actor'}}
  func method() {}
}

func notAnActor<T: IClassValue>(_ value: T) {}
