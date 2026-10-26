// RUN: %target-swift-frontend -sil-verify-all -enable-sil-opaque-values -parse-as-library -Xllvm -sil-print-types -emit-sil -Onone -module-name test %s | %FileCheck %s

public class Klass {}

public struct Wrapper<T> {
  var t: T
  var _k: Klass

  // CHECK-LABEL: sil @$s4test7WrapperV1kAA5KlassCvb : $@convention(method) <T> (@in_guaranteed Wrapper<T>) -> @guaranteed Klass {
  // CHECK:       bb0(%0 : $*Wrapper<T>):
  // CHECK:         [[ADDR:%.*]] = struct_element_addr %0 : $*Wrapper<T>, #Wrapper._k
  // CHECK-NEXT:    [[K:%.*]] = load [[ADDR]] : $*Klass
  // CHECK-NEXT:    return [[K]] : $Klass
  // CHECK-LABEL: } // end sil function '$s4test7WrapperV1kAA5KlassCvb'
  public var k: Klass { borrow { return _k } }
}
