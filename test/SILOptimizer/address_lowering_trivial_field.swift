// RUN: %target-swift-frontend -sil-verify-all -enable-sil-opaque-values -parse-as-library -Xllvm -sil-print-types -emit-sil -Onone -module-name test %s | %FileCheck %s

@available(SwiftStdlib 6.2, *)
public struct Bytes<let n: Int>: ~Copyable {
  var bytes: InlineArray<n, UInt8>

  deinit {}

  // CHECK-LABEL: sil {{.*}}@$s4test5BytesV5firsts11InlineArrayVyxs5UInt8VGvg : $@convention(method) <let n : Int> (@in_guaranteed Bytes<n>) -> @out InlineArray<n, UInt8> {
  // CHECK:       bb0(%0 : $*InlineArray<n, UInt8>, %1 : $*Bytes<n>):
  // CHECK:         [[ADDR:%.*]] = struct_element_addr %1 : $*Bytes<n>, #Bytes.bytes
  // CHECK-NEXT:    copy_addr [[ADDR]] to [init] %0 : $*InlineArray<n, UInt8>
  // CHECK-LABEL: } // end sil function '$s4test5BytesV5firsts11InlineArrayVyxs5UInt8VGvg'
  public var first: InlineArray<n, UInt8> { bytes }
}
