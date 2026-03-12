using TestItemRunner

if VERSION ≥ v"1.9"
    using ForwardMethods
    using Aqua
    Aqua.test_all(ForwardMethods)
end

@run_package_tests verbose=true