struct ManyProperties 
    field1::String 
    field2::Int 
    field3::Bool 
end
@forward_interface ManyProperties interface=getfields 


@test !ForwardMethods.has_defined_interface(SettableProperties, Val(:getfields))
@test !ForwardMethods.has_defined_interface(SettableProperties, Val(:setfields))
@define_interface SettableProperties interface=(getfields, setfields)
@test ForwardMethods.has_defined_interface(SettableProperties, Val(:getfields))
@test ForwardMethods.has_defined_interface(SettableProperties, Val(:setfields))

mutable struct SettablePropertiesNoKey2Key3
    key1::String 
    key2::Int 
    key3::Bool
end
@define_interface SettablePropertiesNoKey2Key3 interface=(getfields, setfields) omit=(key2, key3)

@testset "getfields/setfields" begin 
    p = ManyProperties("abc", 0, true)
    @Test field1(p) == "abc"
    @Test field2(p) == 0
    @Test field3(p) == true

    q = SettableProperties("abc", 0, true)
    @Test key1(q) == "abc"
    @Test key2(q) == 0
    @Test key3(q) == true
    key1!(q, "zzz")
    @Test key1(q) == "zzz"
    key2!(q, 1)
    @Test key2(q) == 1
    key3!(q, false)
    @Test key3(q) == false

    @test hasmethod(key1, Tuple{SettableProperties})
    @test hasmethod(key1!, Tuple{SettableProperties, String})
    @test hasmethod(key2, Tuple{SettableProperties})
    @test hasmethod(key2!, Tuple{SettableProperties, Int})
    @test hasmethod(key3, Tuple{SettableProperties})
    @test hasmethod(key3!, Tuple{SettableProperties, Bool})
    
    @test hasmethod(key1, Tuple{SettablePropertiesNoKey2Key3})
    @test hasmethod(key1!, Tuple{SettablePropertiesNoKey2Key3, String})
    @test !hasmethod(key2, Tuple{SettablePropertiesNoKey2Key3})
    @test !hasmethod(key2!, Tuple{SettablePropertiesNoKey2Key3, Int})
    @test !hasmethod(key3, Tuple{SettablePropertiesNoKey2Key3})
    @test !hasmethod(key3!, Tuple{SettablePropertiesNoKey2Key3, Bool})
end