// RUN: %target-typecheck-verify-swift -enable-builtin-module

import Builtin

func select(_ c: Builtin.Int1, _ a: Builtin.UnsafeValueBuffer, _ b: Builtin.UnsafeValueBuffer) {
  _ = Builtin.select_Int1_UnsafeValueBuffer(c, a, b)
  // expected-error@-1 {{module 'Builtin' has no member named 'select_Int1_UnsafeValueBuffer'}}
}

func vector(_ v: Builtin.Vec2xUnsafeValueBuffer) {}
// expected-error@-1 {{no type named 'Vec2xUnsafeValueBuffer' in module 'Builtin'}}

func intrinsic(_ a: Builtin.UnsafeValueBuffer, _ z: Builtin.Int1) {
  _ = Builtin.int_ctlz_UnsafeValueBuffer(a, z)
  // expected-error@-1 {{module 'Builtin' has no member named 'int_ctlz_UnsafeValueBuffer'}}
  _ = Builtin.int_vscale_UnsafeValueBuffer()
  // expected-error@-1 {{module 'Builtin' has no member named 'int_vscale_UnsafeValueBuffer'}}
}

func scalars(_ c: Builtin.Int1, _ v: Builtin.Vec4xInt32, _ p: Builtin.RawPointer,
             _ f: Builtin.FPIEEE32, _ i: Builtin.Int32) {
  _ = Builtin.select_Int1_Vec4xInt32(c, v, v)
  _ = Builtin.select_Int1_RawPointer(c, p, p)
  _ = Builtin.select_Int1_FPIEEE32(c, f, f)
  _ = Builtin.extractelement_Vec4xInt32_Int32(v, i)
  _ = Builtin.int_ctpop_Int32(i)
}
