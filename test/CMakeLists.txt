list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules")

include(SwiftTestUtils)

function(swift_configure_lit_site_cfg source_path destination_path installed_name)
  if (CMAKE_CFG_INTDIR STREQUAL ".")
    set(SWIFT_BUILD_MODE ".")
  else ()
    set(SWIFT_BUILD_MODE "%(build_mode)s")
  endif ()

  string(REPLACE ${CMAKE_CFG_INTDIR} ${SWIFT_BUILD_MODE} LLVM_TOOLS_DIR ${LLVM_TOOLS_BINARY_DIR})
  string(REPLACE ${CMAKE_CFG_INTDIR} ${SWIFT_BUILD_MODE} LLVM_LIBS_DIR  ${LLVM_LIBRARY_DIR})

  set(LIT_SWIFTLIB_DIR ${SWIFTLIB_DIR})

  configure_file("${source_path}" "${destination_path}" @ONLY)

  if(NOT "${installed_name}" STREQUAL "")
    swift_install_in_component(FILES "${destination_path}"
                               RENAME "${installed_name}"
                               DESTINATION "share/swift/testsuite"
                               COMPONENT testsuite-tools)
  endif()
endfunction()

function(swift_generate_lit_swift_features_cfg output_path)
  if(SWIFT_COMPILER_IS_MSVC_LIKE)
    set(test_lit_swift_features_cmd
      "${CMAKE_C_COMPILER}" "/P" "/EP" "/I" "${SWIFT_MAIN_INCLUDE_DIR}" "/TC"
      "/Fi<<<OUTPUT_FILE>>>")
  else()
    set(test_lit_swift_features_cmd
      "${CMAKE_C_COMPILER}" "-E" "-P" "-I${SWIFT_MAIN_INCLUDE_DIR}" "-x" "c" "-o"
      "<<<OUTPUT_FILE>>>")
  endif()

  list(
    TRANSFORM test_lit_swift_features_cmd
    REPLACE "<<<OUTPUT_FILE>>>" "${output_path}")
  add_custom_command(
    OUTPUT "${output_path}"
    COMMAND
      ${test_lit_swift_features_cmd}
      "${CMAKE_CURRENT_SOURCE_DIR}/lit.swift-features.cfg.inc"
    DEPENDS
      "lit.swift-features.cfg.inc"
      "${SWIFT_MAIN_INCLUDE_DIR}/swift/Basic/Features.def"
  )

  # Execute during generation once, so one can run Lit without rebuilding the
  # test suite dependencies first.
  execute_process(
    COMMAND
      ${test_lit_swift_features_cmd}
      "${CMAKE_CURRENT_SOURCE_DIR}/lit.swift-features.cfg.inc"
  )
endfunction()

function(normalize_boolean_spelling var_name)
  if(${var_name})
    set("${var_name}" TRUE PARENT_SCOPE)
  else()
    set("${var_name}" FALSE PARENT_SCOPE)
  endif()
endfunction()

function(get_test_dependencies SDK result_var_name)
  set(deps)

  if(SWIFT_BUILD_STDLIB)
    list(APPEND deps SwiftUnitTests)
  endif()

  if(SWIFT_BUILD_SDK_OVERLAY OR SWIFT_BUILD_TEST_SUPPORT_MODULES)
    list(APPEND deps sdk-overlay)
  endif()

  set(deps_binaries)

  if (SWIFT_BUILD_LIBEXEC AND SWIFT_ENABLE_BACKTRACING)
    # Only add if `swift-backtrace` builds for this SDK
    if (TARGET "swift-backtrace-${SDK}")
      list(APPEND deps_binaries
        "swift-backtrace-${SDK}")
    endif()
  endif()

  if (SWIFT_INCLUDE_TOOLS)
    list(APPEND deps_binaries
      libMockPlugin
      lldb-moduleimport-test
      swift-frontend
      swift-demangle
      swift-demangle-yamldump
      swift-ide-test
      swift-refactor
      swift-reflection-dump
      swift-remoteast-test
      swift-scan-test)

    if(SWIFT_BUILD_SOURCEKIT)
      list(APPEND deps_binaries sourcekitd-test complete-test)
    endif()

    if(SWIFT_BUILD_SWIFT_SYNTAX)
      list(APPEND deps_binaries swift-plugin-server)
    endif()
  endif()

  if(NOT SWIFT_BUILT_STANDALONE)
    list(APPEND deps_binaries
      c-index-test
      CASPluginTest
      clang
      clang-cas-test
      count
      dsymutil
      FileCheck
      llc
      llvm-ar
      llvm-as
      llvm-bcanalyzer
      llvm-cas
      llvm-cov
      llvm-dis
      llvm-dwarfdump
      llvm-link
      llvm-nm
      llvm-objdump
      llvm-profdata
      llvm-readelf
      llvm-readobj
      llvm-size
      llvm-strings
      llvm-readtapi
      not
      split-file)
  endif()

  if(("${SDK}" STREQUAL "IOS") OR
     ("${SDK}" STREQUAL "TVOS") OR
     ("${SDK}" STREQUAL "WATCHOS") OR
     ("${SDK}" STREQUAL "XROS") OR
     ("${SDK}" STREQUAL "OSX") OR
     ("${SDK}" STREQUAL "IOS_SIMULATOR") OR
     ("${SDK}" STREQUAL "TVOS_SIMULATOR") OR
     ("${SDK}" STREQUAL "WATCHOS_SIMULATOR") OR
     ("${SDK}" STREQUAL "XROS_SIMULATOR") OR
     ("${SDK}" STREQUAL "FREESTANDING") OR
     ("${SDK}" STREQUAL "LINUX") OR
     ("${SDK}" STREQUAL "LINUX_STATIC") OR
     ("${SDK}" STREQUAL "CYGWIN") OR
     ("${SDK}" STREQUAL "FREEBSD") OR
     ("${SDK}" STREQUAL "OPENBSD") OR
     ("${SDK}" STREQUAL "ANDROID") OR
     ("${SDK}" STREQUAL "WINDOWS") OR
     ("${SDK}" STREQUAL "HAIKU") OR
     ("${SDK}" STREQUAL "WASI"))
    # No extra dependencies.
  else()
    message(FATAL_ERROR "Unknown SDK: ${SDK}")
  endif()

  # Just use target names for dependency generation. This works for both Xcode
  # and non-Xcode build systems. In the case of Xcode, its build paths have a
  # configuration variable in them, so CMake can't match them at compile time.
  list(APPEND deps ${deps_binaries})

  set("${result_var_name}" "${deps}" PARENT_SCOPE)
endfunction()

set(LIT "${LLVM_MAIN_SRC_DIR}/utils/lit/lit.py")

set(SWIFT_LIT_ARGS "" CACHE STRING "Arguments to pass to lit")

set(SWIFT_LIT_ENVIRONMENT "" CACHE STRING "Environment to use for lit invocations")

option(SWIFT_TEST_USE_LEAKS "Run Swift stdlib tests under leaks" FALSE)

function(setup_lit_args ARGS_VAR_OUT tested_sdk test_results_dir resource_dir_override)
  set(swift_lit_args_result)

  if(NOT SWIFT_INCLUDE_TOOLS)
    if(SWIFT_RUN_TESTS_WITH_HOST_COMPILER)
      precondition(CMAKE_Swift_COMPILER MESSAGE "Can only run tests if a Swift compiler is specified")
      get_filename_component(SWIFT_COMPILER_DIR "${CMAKE_Swift_COMPILER}" DIRECTORY)
      precondition(SWIFT_COMPILER_DIR)
      # We assume that we are building against a toolchain where all tools are
      # next to swiftc.
      list(APPEND swift_lit_args_result
        "--path=${SWIFT_COMPILER_DIR}")
    else()
      list(APPEND swift_lit_args_result
        "--path=${SWIFT_NATIVE_LLVM_TOOLS_PATH}"
        "--param" "swift_native_llvm_tools_path=${SWIFT_NATIVE_LLVM_TOOLS_PATH}"
        "--path=${SWIFT_NATIVE_CLANG_TOOLS_PATH}"
        "--param" "swift_native_clang_tools_path=${SWIFT_NATIVE_CLANG_TOOLS_PATH}"
        "--path=${SWIFT_NATIVE_SWIFT_TOOLS_PATH}"
        "--param" "swift_native_swift_tools_path=${SWIFT_NATIVE_SWIFT_TOOLS_PATH}"
        )
    endif()
    if(SWIFT_BUILD_STDLIB)
      if(resource_dir_override)
        list(APPEND swift_lit_args_result
             "--param" "test_resource_dir=${resource_dir_override}")
      # If building only static stdlib, use `swift_static` resources directory.
      elseif(NOT SWIFT_BUILD_DYNAMIC_STDLIB AND SWIFT_BUILD_STATIC_STDLIB)
        list(APPEND swift_lit_args_result
             "--param" "test_resource_dir=${SWIFTSTATICLIB_DIR}")
      else()
        list(APPEND swift_lit_args_result
             "--param" "test_resource_dir=${SWIFTLIB_DIR}")
      endif()
    endif()
  endif()

  if (SWIFT_TEST_USE_LEAKS)
    list(APPEND swift_lit_args_result "--param" "leaks-all")
  endif()

  if (SWIFT_ENABLE_ARRAY_COW_CHECKS)
    list(APPEND swift_lit_args_result
         "--param" "array_cow_checks")
  endif()

  if(NOT CMAKE_CFG_INTDIR STREQUAL ".")
    list(APPEND swift_lit_args_result
         "--param" "build_mode=${CMAKE_CFG_INTDIR}")
  endif()

  if(SWIFT_ENABLE_EXPERIMENTAL_DIFFERENTIABLE_PROGRAMMING)
    list(APPEND swift_lit_args_result "--param" "differentiable_programming")
  endif()

  if(SWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY)
    list(APPEND swift_lit_args_result "--param" "concurrency")
  endif()

  if(SWIFT_ENABLE_EXPERIMENTAL_DISTRIBUTED)
    list(APPEND swift_lit_args_result "--param" "distributed")
  endif()

  if(SWIFT_ENABLE_EXPERIMENTAL_STRING_PROCESSING)
    list(APPEND swift_lit_args_result "--param" "string_processing")
  endif()

  if(SWIFT_ENABLE_BACKTRACING)
    list(APPEND swift_lit_args_result "--param" "backtracing")
  endif()

  if(SWIFT_ENABLE_EXPERIMENTAL_OBSERVATION)
    list(APPEND swift_lit_args_result "--param" "observation")
  endif()

  if(SWIFT_ENABLE_SYNCHRONIZATION)
    list(APPEND swift_lit_args_result "--param" "synchronization")
  endif()

  if(SWIFT_ENABLE_VOLATILE)
    list(APPEND swift_lit_args_result "--param" "volatile")
  endif()

  if(SWIFT_ENABLE_RUNTIME_MODULE)
    list(APPEND swift_lit_args_result "--param" "runtime_module")
  endif()

  if(SWIFT_BUILD_REMOTE_MIRROR)
    list(APPEND swift_lit_args_result "--param" "remote_mirror")
  endif()

  list(APPEND swift_lit_args_result "--param" "threading=${SWIFT_SDK_${tested_sdk}_THREADING_PACKAGE}")

  # Enable on-crash backtracing if supported
  if(("${tested_sdk}" STREQUAL "OSX" OR "${tested_sdk}" STREQUAL "LINUX")
      AND NOT SWIFT_ASAN_BUILD)
    list(APPEND swift_lit_args_result "--param" "backtrace_on_crash")
  endif()

  execute_process(COMMAND
      $<TARGET_FILE:Python3::Interpreter> "-c" "import psutil"
      RESULT_VARIABLE python_psutil_status
      TIMEOUT 1 # second
      ERROR_QUIET)
  if(NOT python_psutil_status)
    list(APPEND swift_lit_args_result "--timeout=3000") # 50 minutes
  endif()

  list(APPEND swift_lit_args_result "--xunit-xml-output=${test_results_dir}/lit-tests.xml")

  if(NOT SWIFT_BUILD_STDLIB AND NOT SWIFT_PATH_TO_EXTERNAL_STDLIB_BUILD)
    list(APPEND swift_lit_args_result
        "--param" "test_sdk_overlay_dir=${SWIFTLIB_DIR}/${SWIFT_SDK_${tested_sdk}_LIB_SUBDIR}")
  endif()


  set(swift_lit_args_result "${swift_lit_args_result} ${SWIFT_LIT_ARGS} ${LLVM_LIT_ARGS}")
  separate_arguments(swift_lit_args_result)

  set(${ARGS_VAR_OUT} ${swift_lit_args_result} PARENT_SCOPE)
endfunction()

if (LLVM_USE_SANITIZER STREQUAL "Address")
  set(SWIFT_ASAN_BUILD TRUE)
endif()

# Normalize spelling of boolean values.
normalize_boolean_spelling(LLVM_ENABLE_ASSERTIONS)
normalize_boolean_spelling(SWIFT_STDLIB_ASSERTIONS)
normalize_boolean_spelling(SWIFT_AST_VERIFIER)
normalize_boolean_spelling(SWIFT_ASAN_BUILD)
normalize_boolean_spelling(SWIFT_ENABLE_SOURCEKIT_TESTS)
normalize_boolean_spelling(SWIFT_ENABLE_EXPERIMENTAL_DIFFERENTIABLE_PROGRAMMING)
normalize_boolean_spelling(SWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY)
normalize_boolean_spelling(SWIFT_ENABLE_EXPERIMENTAL_DISTRIBUTED)
normalize_boolean_spelling(SWIFT_ENABLE_EXPERIMENTAL_STRING_PROCESSING)
normalize_boolean_spelling(SWIFT_ENABLE_EXPERIMENTAL_OBSERVATION)
normalize_boolean_spelling(SWIFT_ENABLE_EXPERIMENTAL_POINTER_BOUNDS)
normalize_boolean_spelling(SWIFT_ENABLE_MACCATALYST)
normalize_boolean_spelling(SWIFT_RUN_TESTS_WITH_HOST_COMPILER)
normalize_boolean_spelling(SWIFT_RUNTIME_ENABLE_LEAK_CHECKER)
normalize_boolean_spelling(SWIFT_OPTIMIZED)
normalize_boolean_spelling(SWIFT_STDLIB_SINGLE_THREADED_CONCURRENCY)
normalize_boolean_spelling(SWIFT_ENABLE_REFLECTION)
normalize_boolean_spelling(SWIFT_ENABLE_RUNTIME_FUNCTION_COUNTERS)
normalize_boolean_spelling(SWIFT_STDLIB_ENABLE_DEBUG_PRECONDITIONS_IN_RELEASE)
normalize_boolean_spelling(SWIFT_HAVE_LIBXML2)
normalize_boolean_spelling(SWIFT_INCLUDE_TOOLS)
normalize_boolean_spelling(SWIFT_STDLIB_STATIC_PRINT)
normalize_boolean_spelling(SWIFT_STDLIB_ENABLE_UNICODE_DATA)
normalize_boolean_spelling(SWIFT_ENABLE_DISPATCH)
normalize_boolean_spelling(SWIFT_STDLIB_ENABLE_OBJC_INTEROP)
normalize_boolean_spelling(SWIFT_ENABLE_BACKTRACING)
normalize_boolean_spelling(SWIFT_ENABLE_RUNTIME_MODULE)
normalize_boolean_spelling(SWIFT_BUILD_SWIFT_SYNTAX)
normalize_boolean_spelling(SWIFT_ENABLE_SYNCHRONIZATION)
normalize_boolean_spelling(SWIFT_ENABLE_VOLATILE)
normalize_boolean_spelling(SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB)
normalize_boolean_spelling(SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB_CROSS_COMPILING)
normalize_boolean_spelling(SWIFT_BUILD_REMOTE_MIRROR)
is_build_type_optimized("${SWIFT_STDLIB_BUILD_TYPE}" SWIFT_OPTIMIZED)

# Get 'SWIFT_HOST_SDKROOT' for lit.site.cfg.in
set(SWIFT_HOST_SDKROOT "${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_ARCH_${SWIFT_HOST_VARIANT_ARCH}_PATH}")

set(profdata_merge_worker
    "${CMAKE_CURRENT_SOURCE_DIR}/../utils/profdata_merge/main.py")

set(TEST_MODES
    optimize_none optimize optimize_unchecked optimize_size
    optimize_none_with_implicit_dynamic
    optimize_with_implicit_dynamic
    only_executable only_non_executable
    with_cxx_interop
)
set(TEST_SUBSETS
    primary
    validation
    all
    only_validation
    only_long
    only_stress
)

if(NOT "${COVERAGE_DB}" STREQUAL "")
  add_custom_target("touch-covering-tests"
      COMMAND "${SWIFT_SOURCE_DIR}/utils/coverage/coverage-touch-tests" "--swift-dir" "${SWIFT_SOURCE_DIR}" "--coverage-db" "${COVERAGE_DB}"
      COMMENT "Touching covering tests")
endif()

foreach(SDK ${SWIFT_SDKS})
  foreach(ARCH ${SWIFT_SDK_${SDK}_ARCHITECTURES})
    # macCatalyst needs to run two sets of tests: one with the normal macosx target triple
    # and one with the macCatalyst ios-macabi triple.  The build_flavors list will
    # have only the "default" flavor for all SDKs and architectures except
    # OSX when macCatalyst support is enabled.
    get_swift_test_build_flavors(build_flavors "${SDK}")

    foreach(BUILD_FLAVOR ${build_flavors})
      # Configure variables for this subdirectory.
      set(VARIANT_SDK "${SWIFT_SDK_${SDK}_ARCH_${ARCH}_PATH}")
      get_swift_test_variant_suffix(VARIANT_SUFFIX "${SDK}" "${ARCH}" "${BUILD_FLAVOR}")
      get_swift_test_variant_suffix(DEFAULT_OSX_VARIANT_SUFFIX "${SDK}" "${ARCH}" "default")
      get_swift_test_versioned_target_triple(VARIANT_TRIPLE "${SDK}" "${ARCH}" "${SWIFT_SDK_${SDK}_DEPLOYMENT_VERSION}" "${BUILD_FLAVOR}")

      # A directory where to put the xUnit-style XML test results.
      set(SWIFT_TEST_RESULTS_DIR
          "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/swift-test-results/${VARIANT_TRIPLE}")

      set(command_clean_test_results_dir
          COMMAND "${CMAKE_COMMAND}" -E remove_directory "${SWIFT_TEST_RESULTS_DIR}"
          COMMAND "${CMAKE_COMMAND}" -E make_directory "${SWIFT_TEST_RESULTS_DIR}")

      set(test_bin_dir "${CMAKE_CURRENT_BINARY_DIR}${VARIANT_SUFFIX}")
      set(validation_test_bin_dir
          "${CMAKE_CURRENT_BINARY_DIR}/../validation-test${VARIANT_SUFFIX}")

      if(LLVM_ENABLE_LIBXML2)
        set(SWIFT_HAVE_LIBXML2 TRUE)
      else()
        set(SWIFT_HAVE_LIBXML2 FALSE)
      endif()

      set(VARIANT_EXTERNAL_EMBEDDED_PLATFORM FALSE)
      set(VARIANT_EXTERNAL_EMBEDDED_DEVICE)

      # Reset these values for WASI, as Embedded Swift for WASI requires separate lit configs.
      set(SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB_old ${SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB})
      set(SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB_CROSS_COMPILING_old ${SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB_CROSS_COMPILING})
      if("${SDK}" STREQUAL "WASI")
        set(SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB OFF)
        set(SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB_CROSS_COMPILING OFF)
      endif()

      swift_configure_lit_site_cfg(
          "${CMAKE_CURRENT_SOURCE_DIR}/lit.site.cfg.in"
          "${test_bin_dir}/lit.site.cfg"
          "test${VARIANT_SUFFIX}.lit.site.cfg")

      swift_configure_lit_site_cfg(
          "${CMAKE_CURRENT_SOURCE_DIR}/Unit/lit.site.cfg.in"
          "${test_bin_dir}/Unit/lit.site.cfg"
          "")

      swift_configure_lit_site_cfg(
          "${CMAKE_CURRENT_SOURCE_DIR}/../validation-test/lit.site.cfg.in"
          "${validation_test_bin_dir}/lit.site.cfg"
          "validation-test${VARIANT_SUFFIX}.lit.site.cfg")

      swift_generate_lit_swift_features_cfg("${test_bin_dir}/lit.swift-features.cfg")
      swift_generate_lit_swift_features_cfg("${validation_test_bin_dir}/lit.swift-features.cfg")
      add_custom_target(lit_swift_features_cfg_${VARIANT_SUFFIX}
        DEPENDS
          "${test_bin_dir}/lit.swift-features.cfg"
          "${validation_test_bin_dir}/lit.swift-features.cfg")

      # Restore old values in case they were reset for WASI tests lit configs.
      set(SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB ${SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB_old})
      set(SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB_CROSS_COMPILING ${SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB_CROSS_COMPILING_old})

      set(test_dependencies)
      get_test_dependencies("${SDK}" test_dependencies)

      # Keep in sync with stdlib/tools/CMakeLists.txt: swift-reflection-test is
      # only used when testing dynamic stdlib.
      if(SWIFT_BUILD_DYNAMIC_STDLIB AND SWIFT_INCLUDE_TESTS)
        list(APPEND test_dependencies BlocksRuntimeStub${VARIANT_SUFFIX})

        list(APPEND test_dependencies
            "swift-test-stdlib-${SWIFT_SDK_${SDK}_LIB_SUBDIR}")

        if(BUILD_FLAVOR STREQUAL "ios-like")
          # When testing the iOS-like build flavor, use the normal macOS
          # swift-reflection-test-tool. That tool runs out of process so it
          # doesn't need to be build for macCatalyst.
          list(APPEND test_dependencies
              "swift-reflection-test${DEFAULT_OSX_VARIANT_SUFFIX}")
        else()
          list(APPEND test_dependencies
              "swift-reflection-test${VARIANT_SUFFIX}")
        endif()
      endif()

      if(TARGET "embedded-libraries")
        list(APPEND test_dependencies "embedded-libraries")
      endif()

      if(NOT "${COVERAGE_DB}" STREQUAL "")
        list(APPEND test_dependencies "touch-covering-tests")
      endif()

      list(APPEND test_dependencies
        "swiftStdlibCollectionUnittest-${SWIFT_SDK_${SDK}_LIB_SUBDIR}"
        "swiftStdlibUnicodeUnittest-${SWIFT_SDK_${SDK}_LIB_SUBDIR}")

      set(validation_test_dependencies)

      list(APPEND test_dependencies lit_swift_features_cfg_${VARIANT_SUFFIX})
      list(APPEND validation_test_dependencies lit_swift_features_cfg_${VARIANT_SUFFIX})

      set(command_upload_stdlib)
      set(command_upload_swift_reflection_test)
      if("${SDK}" STREQUAL "IOS" OR "${SDK}" STREQUAL "TVOS" OR "${SDK}" STREQUAL "WATCHOS" OR "${SDK}" STREQUAL "XROS")
        # These are supported testing SDKs, but their implementation of
        # `command_upload_stdlib` is hidden.
      elseif("${SDK}" STREQUAL "ANDROID" AND NOT "${SWIFT_HOST_VARIANT}" STREQUAL "android")
        # This adb setup is only needed when cross-compiling for Android, so the
        # second check above makes sure we don't bother when the host is Android.
        if("${SWIFT_ANDROID_DEPLOY_DEVICE_PATH}" STREQUAL "")
          message(FATAL_ERROR
              "When running Android host tests, you must specify the directory on the device "
              "to which Swift build products will be deployed.")
        endif()

        # Warning: This step will fail if you do not have an Android device
        #          connected via USB. See docs/Android.md for details on
        #          how to run the test suite for Android.
        set(command_upload_stdlib
            COMMAND
                # Reboot the device and remove everything in its tmp
                # directory. Build products and test executables are pushed
                # to that directory when running the test suite.
                $<TARGET_FILE:Python3::Interpreter> "${SWIFT_SOURCE_DIR}/utils/android/adb_clean.py"
            COMMAND
                $<TARGET_FILE:Python3::Interpreter> "${SWIFT_SOURCE_DIR}/utils/android/adb_push_built_products.py"
                --ndk "${SWIFT_ANDROID_NDK_PATH}"
                --destination "${SWIFT_ANDROID_DEPLOY_DEVICE_PATH}"
                --destination-arch "${ARCH}"
                # Build products like libswiftCore.so.
                "${SWIFTLIB_DIR}/android")
      endif()
      add_custom_target("upload-stdlib${VARIANT_SUFFIX}"
          ${command_upload_stdlib}
          ${command_upload_swift_reflection_test}
          COMMENT "Uploading stdlib")

      foreach(test_mode ${TEST_MODES})
        foreach(test_subset ${TEST_SUBSETS})
          set(directories)
          set(dependencies ${test_dependencies})

          if((test_subset STREQUAL "primary") OR
             (test_subset STREQUAL "validation") OR
             (test_subset STREQUAL "only_long") OR
             (test_subset STREQUAL "only_stress") OR
             (test_subset STREQUAL "all"))
            list(APPEND directories "${test_bin_dir}")
          endif()
          if((test_subset STREQUAL "validation") OR
             (test_subset STREQUAL "only_validation") OR
             (test_subset STREQUAL "only_long") OR
             (test_subset STREQUAL "only_stress") OR
             (test_subset STREQUAL "all"))
            list(APPEND directories "${validation_test_bin_dir}")
            list(APPEND dependencies ${validation_test_dependencies})
          endif()

          if("${SWIFT_SDK_${SDK}_OBJECT_FORMAT}" STREQUAL "ELF")
            list(APPEND dependencies swiftImageRegistration${VARIANT_SUFFIX})
          endif()

          set(test_subset_target_suffix "-${test_subset}")
          if(test_subset STREQUAL "primary")
            set(test_subset_target_suffix "")
          endif()

          set(test_mode_target_suffix "")
          if(NOT test_mode STREQUAL "optimize_none")
            set(test_mode_target_suffix "-${test_mode}")
          endif()

          set(maybe_command_upload_stdlib)
          if(NOT test_mode STREQUAL "only_non_executable")
            set(maybe_command_upload_stdlib ${command_upload_stdlib})
          endif()

          setup_lit_args(final_lit_args "${SDK}" "${SWIFT_TEST_RESULTS_DIR}" OFF)

          set(test_target_name
              "check-swift${test_subset_target_suffix}${test_mode_target_suffix}${VARIANT_SUFFIX}")
          add_custom_target("${test_target_name}"
              ${maybe_command_upload_stdlib}
              ${command_upload_swift_reflection_test}
              ${command_clean_test_results_dir}
              COMMAND
                ${CMAKE_COMMAND} -E env ${SWIFT_LIT_ENVIRONMENT}
                $<TARGET_FILE:Python3::Interpreter> "${LIT}"
                ${final_lit_args}
                "--param" "swift_test_subset=${test_subset}"
                "--param" "swift_test_mode=${test_mode}"
                ${directories}
              DEPENDS ${dependencies}
              COMMENT "Running ${test_subset} Swift tests for ${VARIANT_TRIPLE}"
              USES_TERMINAL)

          set(test_dependencies_target_name
              "swift${test_subset_target_suffix}${test_mode_target_suffix}${VARIANT_SUFFIX}-test-depends")
          add_custom_target("${test_dependencies_target_name}"
              DEPENDS ${dependencies})

          add_custom_target("${test_target_name}-custom"
              ${command_upload_stdlib}
              ${command_upload_swift_reflection_test}
              ${command_clean_test_results_dir}
              COMMAND
                ${CMAKE_COMMAND} -E env ${SWIFT_LIT_ENVIRONMENT}
                $<TARGET_FILE:Python3::Interpreter> "${LIT}"
                ${final_lit_args}
                "--param" "swift_test_subset=${test_subset}"
                "--param" "swift_test_mode=${test_mode}"
                ${SWIFT_LIT_TEST_PATHS}
              DEPENDS ${dependencies}
              COMMENT "Running ${test_subset} Swift tests for ${VARIANT_TRIPLE} from custom test locations"
              USES_TERMINAL)
          set_property(TARGET
              "${test_target_name}"
              "${test_target_name}-custom"
              "${test_dependencies_target_name}"
              PROPERTY FOLDER "Tests/check-swift")
        endforeach()
      endforeach()

      if(SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB_CROSS_COMPILING AND (${ARCH} STREQUAL "wasm32"))
        set(test_mode "optimize_none")
        set(test_subset "primary")
        set(VARIANT_SUFFIX "-embedded-wasi")
        set(VARIANT_TRIPLE "wasm32-unknown-wasip1")
        set(test_bin_dir "${CMAKE_CURRENT_BINARY_DIR}${VARIANT_SUFFIX}/embedded")
        set(VARIANT_EXTERNAL_EMBEDDED_PLATFORM FALSE)
        set(VARIANT_EXTERNAL_EMBEDDED_DEVICE)
        set(SWIFT_TEST_RESULTS_DIR "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/swift-test-results/${VARIANT_TRIPLE}")
        swift_configure_lit_site_cfg(
            "${CMAKE_CURRENT_SOURCE_DIR}/lit.site.cfg.in"
            "${CMAKE_CURRENT_BINARY_DIR}${VARIANT_SUFFIX}/lit.site.cfg"
            "test${VARIANT_SUFFIX}.lit.site.cfg")
        swift_generate_lit_swift_features_cfg("${CMAKE_CURRENT_BINARY_DIR}${VARIANT_SUFFIX}/lit.swift-features.cfg")

        set(test_target_name "check-swift${VARIANT_SUFFIX}")
        set(directories "${test_bin_dir}")

        set(embedded_wasm_lit_args)
        setup_lit_args(embedded_wasm_lit_args "${SDK}" "${SWIFT_TEST_RESULTS_DIR}" "${SWIFTLIB_DIR}")

        add_custom_target("${test_target_name}"
            ${maybe_command_upload_stdlib}
            ${command_upload_swift_reflection_test}
            ${command_clean_test_results_dir}
            COMMAND
              ${CMAKE_COMMAND} -E env ${SWIFT_LIT_ENVIRONMENT}
              $<TARGET_FILE:Python3::Interpreter> "${LIT}"
              ${embedded_wasm_lit_args}
              "--param" "swift_test_subset=${test_subset}"
              "--param" "swift_test_mode=${test_mode}"
              ${directories}
            DEPENDS ${dependencies}
            COMMENT "Running ${test_subset} Swift tests for ${VARIANT_TRIPLE}"
            USES_TERMINAL)
        endif()
    endforeach()
  endforeach()
endforeach()

if(SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB_CROSS_COMPILING)
  set(VARIANT_SUFFIX "-embedded-armv7")
  set(VARIANT_TRIPLE "armv7em-none-none-eabi")
  set(VARIANT_EXTERNAL_EMBEDDED_PLATFORM TRUE)
  set(VARIANT_EXTERNAL_EMBEDDED_DEVICE "arm-qemu-stm32f4")
  set(SWIFT_TEST_RESULTS_DIR "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/swift-test-results/${VARIANT_TRIPLE}")
  swift_configure_lit_site_cfg(
      "${CMAKE_CURRENT_SOURCE_DIR}/lit.site.cfg.in"
      "${CMAKE_CURRENT_BINARY_DIR}${VARIANT_SUFFIX}/lit.site.cfg"
      "test${VARIANT_SUFFIX}.lit.site.cfg")
  swift_generate_lit_swift_features_cfg("${CMAKE_CURRENT_BINARY_DIR}${VARIANT_SUFFIX}/lit.swift-features.cfg")

  set(VARIANT_SUFFIX "-embedded-avr")
  set(VARIANT_TRIPLE "avr-none-none-elf")
  set(VARIANT_EXTERNAL_EMBEDDED_PLATFORM TRUE)
  set(VARIANT_EXTERNAL_EMBEDDED_DEVICE "avr-qemu-atmega2560")
  set(SWIFT_TEST_RESULTS_DIR "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/swift-test-results/${VARIANT_TRIPLE}")
  swift_configure_lit_site_cfg(
      "${CMAKE_CURRENT_SOURCE_DIR}/lit.site.cfg.in"
      "${CMAKE_CURRENT_BINARY_DIR}${VARIANT_SUFFIX}/lit.site.cfg"
      "test${VARIANT_SUFFIX}.lit.site.cfg")
  swift_generate_lit_swift_features_cfg("${CMAKE_CURRENT_BINARY_DIR}${VARIANT_SUFFIX}/lit.swift-features.cfg")

  set(VARIANT_SUFFIX "-embedded-riscv32")
  set(VARIANT_TRIPLE "riscv32-none-none-eabi")
  set(VARIANT_EXTERNAL_EMBEDDED_PLATFORM TRUE)
  set(VARIANT_EXTERNAL_EMBEDDED_DEVICE "riscv32-qemu-virt")
  set(SWIFT_TEST_RESULTS_DIR "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/swift-test-results/${VARIANT_TRIPLE}")
  swift_configure_lit_site_cfg(
      "${CMAKE_CURRENT_SOURCE_DIR}/lit.site.cfg.in"
      "${CMAKE_CURRENT_BINARY_DIR}${VARIANT_SUFFIX}/lit.site.cfg"
      "test${VARIANT_SUFFIX}.lit.site.cfg")
  swift_generate_lit_swift_features_cfg("${CMAKE_CURRENT_BINARY_DIR}${VARIANT_SUFFIX}/lit.swift-features.cfg")
endif()

# Add shortcuts for the default variant.
foreach(test_mode ${TEST_MODES})
  foreach(test_subset ${TEST_SUBSETS})
    set(test_mode_target_suffix "")
    if(NOT test_mode STREQUAL "optimize_none")
      set(test_mode_target_suffix "-${test_mode}")
    endif()
    set(test_subset_target_suffix "-${test_subset}")
    if(test_subset STREQUAL "primary")
      set(test_subset_target_suffix "")
    endif()

    set(test_target_name
        "check-swift${test_subset_target_suffix}${test_mode_target_suffix}")
    add_custom_target("${test_target_name}"
        DEPENDS "${test_target_name}${SWIFT_PRIMARY_VARIANT_SUFFIX}")
    set_property(TARGET "${test_target_name}"
        PROPERTY FOLDER "Tests/check-swift")

    set(test_depends_target_name
        "swift${test_subset_target_suffix}${test_mode_target_suffix}-test-depends")
    add_custom_target("${test_depends_target_name}"
        DEPENDS "swift${test_subset_target_suffix}${test_mode_target_suffix}${SWIFT_PRIMARY_VARIANT_SUFFIX}-test-depends")
    set_property(TARGET "${test_depends_target_name}"
        PROPERTY FOLDER "Tests/check-swift")
  endforeach()
endforeach()
