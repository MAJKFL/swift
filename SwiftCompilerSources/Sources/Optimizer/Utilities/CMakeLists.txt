# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for Swift project authors

swift_compiler_sources(Optimizer
  AddressUtils.swift
  SpecializationCloner.swift
  Devirtualization.swift
  EscapeUtils.swift
  FunctionSignatureTransforms.swift
  FunctionTest.swift
  GenericSpecialization.swift
  LifetimeDependenceUtils.swift
  LocalVariableUtils.swift
  OptUtils.swift
  OwnershipLiveness.swift
  StaticInitCloner.swift
)
