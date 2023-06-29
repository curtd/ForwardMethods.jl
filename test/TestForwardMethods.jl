module TestForwardMethods 
    using ForwardMethods, ForwardMethods.MLStyle 

    using Test, TestingUtilities 

    macro test_throws_compat(ExceptionType, message, expr)
        output = Expr(:block)
    
        push!(output.args, :(@test_throws $ExceptionType $expr))
        if VERSION ≥ v"1.7"
            push!(output.args, :(@test_throws $message $expr))
        end
        return output |> esc
    end

    struct A
        v::Vector{Int}
    end
    @forward_methods A field=v Base.length(x::A) Base.getindex(_, k) Base.eltype(::Type{A})

    test_func(v::Vector) = v[1]

    struct B{T}
        v::Vector{T}
    end
    @forward_methods B{T} field=getfield(b,:v) Base.length(x::B) (Base.getindex(_, k) where {T}) test_func

    struct ForwardDict
        d::Dict{String,Int}
    end
    @forward_interface ForwardDict field=d interface=dict

    struct ForwardVector{T}
        v::Vector{T}
    end
    @forward_interface ForwardVector{T} field=v interface=array index_style_linear=true 

    struct ForwardVectorNoLength{T}
        v::Vector{T}
    end
    @forward_interface ForwardVectorNoLength{T} field=v interface=array index_style_linear=true omit=[length]

    struct ForwardMatrix{F}
        v::Matrix{F}
    end
    @forward_interface ForwardMatrix{F} field=v interface=array index_style_linear=false 

    struct LockableDict{K,V}
        d::Dict{K,V}
        lock::ReentrantLock
    end
    @forward_interface LockableDict{K,V} field=lock interface=lockable
    @forward_interface LockableDict{K,V} field=d interface=dict map=begin lock(_obj); try _ finally unlock(_obj) end end

    struct InnerVector 
        v::Vector{Int}
    end
    struct NestedForward
        h::InnerVector
    end
    @forward_interface NestedForward field=h.v interface=array index_style_linear=true

    struct ManyProperties 
        key1::String 
        key2::Int 
        key3::Bool 
    end
    @forward_interface ManyProperties interface=getfields 

    mutable struct SettableProperties 
        key1::String 
        key2::Int 
        key3::Bool
    end
    @forward_interface SettableProperties interface=(getfields,setfields)

    mutable struct CompositeProperties
        settable::SettableProperties
        key4::Float64
    end
    if VERSION ≥ v"1.7"
        @define_interface CompositeProperties interface=properties delegated_fields=settable
    else
        @define_interface CompositeProperties interface=properties delegated_fields=settable is_mutable=true
    end

    mutable struct SettableProperties2
        key4::Float64
        key5::Vector{Int}
    end
    @define_interface SettableProperties2 interface=equality 

    struct EqualityUsingProperties 
        d::Dict{Symbol,Int}
    end
    Base.propertynames(e::EqualityUsingProperties) = collect(keys(getfield(e, :d)))
    Base.getproperty(e::EqualityUsingProperties, k::Symbol) = getfield(e, :d)[k]
    Base.setproperty!(e::EqualityUsingProperties, k::Symbol, v::Int) = getfield(e, :d)[k] = v
    @define_interface EqualityUsingProperties interface=equality compare_fields=propertynames

    mutable struct CompositeProperties2
        settable::SettableProperties
        alsosettable::SettableProperties2
    end
    if VERSION ≥ v"1.7"
        @define_interface CompositeProperties2 interface=properties delegated_fields=(settable, alsosettable)
    else
        @define_interface CompositeProperties2 interface=properties delegated_fields=(settable, alsosettable) is_mutable=true
    end

    struct ClashingKeys 
        key1::SettableProperties
        key2::String
    end
    @test_throws_compat ErrorException "Type $ClashingKeys has duplicate field names between itself and/or its requested fields (= (:key1,)) -- duplicate field names = [:key1, :key2]" @define_interface ClashingKeys interface=properties delegated_fields=key1

    @testset "@forward" begin 
        @testset "Parsing" begin 
            @test_cases begin 
                input                        |  output_x                | output_y                  
                :(field=getproperty(_,:abc)) | :(Base.getproperty(x, :abc))  | :(Base.getproperty(y, :abc))
                :(field=Base.getproperty(_,:abc)) |  :(Base.getproperty(x, :abc))  | :(Base.getproperty(y, :abc)) 
                :(field=getindex(x,:k))      | :(Base.getindex(x,:k)) | :(Base.getindex(y, :k)) 
                :(field=t[])                 | :(x[])       | :(y[])
                :(field=t[1])                 | :(x[1])       | :(y[1])
                :(field=f(z))                | :(f(x)) | :(f(y)) 
                @test (ForwardMethods.parse_field(input).arg_func)(:x) == output_x
                @test (ForwardMethods.parse_field(input).arg_func)(:y) == output_y
                @test isnothing(ForwardMethods.parse_field(input).type_func)
            end
            @test_cases begin 
                input                        | output_arg_x             | output_arg_y               | output_type_x
                :(field=:abc)                | :(Base.getfield(x,:abc)) | :(Base.getfield(y,:abc))   | :(Base.fieldtype(x, :abc))
                :(field=abc)                 | :(Base.getfield(x,:abc)) | :(Base.getfield(y,:abc))   | :(Base.fieldtype(x, :abc))
                :(field=abc.def.ghi)         | :(Base.getfield(Base.getfield(Base.getfield(x,:abc), :def), :ghi)) | :(Base.getfield(Base.getfield(Base.getfield(y,:abc), :def), :ghi))   |  :(Base.fieldtype(Base.fieldtype(Base.fieldtype(x,:abc), :def), :ghi))
                :(field=getfield(_,:abc))    | :(Base.getfield(x,:abc)) | :(Base.getfield(y,:abc))   | :(Base.fieldtype(x, :abc))
                :(field=Base.getfield(_,:abc)) | :(Base.getfield(x,:abc)) | :(Base.getfield(y,:abc)) | :(Base.fieldtype(x, :abc))
                @test (ForwardMethods.parse_field(input).arg_func)(:x) == output_arg_x
                @test (ForwardMethods.parse_field(input).arg_func)(:y) == output_arg_y
                @test (ForwardMethods.parse_field(input).type_func)(:x) == output_type_x
            end
            @test_cases begin 
                Type        |  UnionallType  |   expr    |  output 
                :A          |  :A            | :(x::A)   | (; matches=true, unionall=false, type=false, arg=:x)
                :A          |  :A            | :(x::Type{A})   | (; matches=true, unionall=false, type=true, arg=:A)
                :(A{B,C})          |  :A            | :(x::Type{A})   | (; matches=true, unionall=true, type=true, arg=:A)
                :(A{B,C})          |  :A            | :(x::A{B,C})   | (; matches=true, unionall=false, type=false, arg=:x)
                :(A{B,C})          |  :A            | :(x::Type{A{B,C}})   | (; matches=true, unionall=false, type=true, arg=:(A{B,C}))
                @test ForwardMethods.matches_type_signature(Type, UnionallType, expr) == output
            end
            @test_cases begin 
                input        |  replace_values  |  output 
                (input=:_ , replace_values=[:_ => :t], output=(:t, true))
                (input=:(f(_)), replace_values=[:_ => :t], output=(:(f(t)), true))
                (input=:(f(x)), replace_values=[:_ => :t], output=(:(f(x)), false))
                (input=:(f(x)), replace_values=[:_ => :t, :x => :s], output=(:(f(s)), true))
                @test ForwardMethods.replace_placeholder(input, replace_values) == output
            end

            ref_obj = :x
            ref_expr = :(Base.iterate(Base.getfield(x,k)))
            @test_cases begin 
                input             |  output  
                :(map=_)          |  ref_expr 
                :(map=f(_))      |  :(f($ref_expr))
                :(map=begin z = f(_); g(z) end) | Expr(:block, :(z = f($ref_expr)), :(g(z)))
                :(map=begin z = f(_); g(z, _obj) end) | Expr(:block, :(z = f($ref_expr)), :(g(z, $ref_obj)))
                @test ForwardMethods.parse_map_expr(input)(ref_obj, ref_expr) == output
            end
            @Test isnothing(ForwardMethods.parse_map_expr(:x))
        end
        @testset "Expression generation" begin 
            field_funcs = ForwardMethods.FieldFuncExprs( (t)->:(Base.getproperty($t, :b)), nothing )
            matches_rhs = (t, x_ref=:x)->begin 
                @match t begin 
                    quote 
                        local $var1 = Base.getproperty($var2, :b)
                        Base.getindex($var3, k)
                    end => (var2 == x_ref && var1 == var3 )
                    _ => false
                end
            end
            for (T, input_expr) in ((:A, :(Base.getindex(x::A, k))), (:(A{B1,B2}), :(Base.getindex(x::A{B1,B2},k) where {B1,B2})), (:(A{B1,B2}), :(Base.getindex(x::A, k))))
                output = ForwardMethods.forward_method_signature(T, field_funcs, input_expr)
                matches = @switch output begin
                    @case :($lhs = $rhs) && if lhs == input_expr end 
                        matches_rhs(rhs)
                    @case _ 
                        false
                end
                @test matches
            end

            field_funcs = ForwardMethods.FieldFuncExprs( (t)->:(Base.getfield($t, :b)), (t) -> :(Base.fieldtype($t, :b)))
            T = :A 
            input_expr = :(Base.length) 
            matches_rhs = (t, x_ref=:x)->begin 
                @match t begin 
                    quote 
                        local $var1 = Base.getfield($var2, :b)
                        Base.length($var3)
                    end => (var2 == x_ref && var1 == var3 )
                    _ => false
                end
            end
            output = ForwardMethods.forward_method_signature(T, field_funcs, input_expr)
            matches = @switch output begin
                @case :($lhs = $rhs)
                    @match lhs begin 
                        :(Base.length($var1::A)) => matches_rhs(rhs, var1)
                        _ => false
                    end
                @case _ 
                    false
            end
            @test matches

            input_expr = :(Base.eltype(::Type{A})) 
            matches_rhs = (t, x_ref=:x)->begin 
                @match t begin 
                    quote 
                        local $var1 = Base.fieldtype($var2, :b)
                        Base.eltype($var3)
                    end => (var2 == x_ref && var1 == var3 )
                    _ => false
                end
            end
            output = ForwardMethods.forward_method_signature(T, field_funcs, input_expr)
            matches = @switch output begin
                @case :($lhs = $rhs)
                    @match lhs begin 
                        :(Base.eltype(::Type{A})) => matches_rhs(rhs, T)
                        _ => false
                    end
                @case _ 
                    false
            end
            @test matches
            
            # Testing underscore parameters 
            field_funcs = ForwardMethods.FieldFuncExprs( (t)->:(Base.getfield($t, :b)), (t) -> :(Base.fieldtype($t, :b)))
            T = :(A{B1,B2})  
            input_expr = :(Base.getindex(_, k) where {B1, B2})
            matches_rhs = (t, x_ref=:x)->begin 
                @match t begin 
                    quote 
                        local $var1 = Base.getfield($var2, :b)
                        Base.getindex($var3, k)
                    end => (var2 == x_ref && var1 == var3 )
                    _ => false
                end
            end
            output = ForwardMethods.forward_method_signature(T, field_funcs, input_expr)
            matches = @switch output begin
                @case :($lhs = $rhs)
                    @match lhs begin 
                        :(Base.getindex($var1::A{B1,B2}, k) where {B1,B2}) => matches_rhs(rhs, var1)
                        _ => false
                    end
                @case _ 
                    false
            end
            @test matches

            
            # If provided type is parametric but signature does not qualify where statement -- use unionall type in signature
            T = :(C.A{B1,B2})  
            input_expr = :(Base.getindex(_, k))
            output = ForwardMethods.forward_method_signature(T, field_funcs, input_expr)
            matches = @switch output begin
                @case :($lhs = $rhs)
                    @match lhs begin 
                        :(Base.getindex($var1::C.A, k)) => matches_rhs(rhs, var1)
                        _ => false
                    end
                @case _ 
                    false
            end
            @test matches
        end

        @Test length(A([0])) == 1
        @Test A([0])[1] == 0
        @Test eltype(A) == Int

        @Test length(B([0])) == 1
        @Test B([0])[1] == 0

        v = NestedForward(InnerVector([1,2,3]))
        @Test v[1] == 1
    end

    @testset "@forward_interface" begin 
        d = ForwardDict(Dict{String, Int}())
        @Test isempty(d)
        @Test !haskey(d, "a")
        @Test isnothing(pop!(d, "a", nothing))
        @Test isnothing(get(d, "a", nothing))
        d["a"] = 1
        d["b"] = 2
        @Test !isempty(d)
        @Test haskey(d, "a")
        @Test d["a"] == 1
        @Test keytype(typeof(d)) == String
        @Test valtype(typeof(d)) == Int
        @Test eltype(typeof(d)) == Pair{String, Int}
        @Test ("a" => 1) in d
        @Test "a" ∈ keys(d)
        @Test Set(collect(pairs(d))) == Set(["a" => 1, "b" => 2])
        @Test 1 ∈ values(d)
        empty!(d)
        @Test isempty(d)
        
        f = ForwardVector(Int[])
        @Test isempty(f)
        @Test size(f) == (0,)
        @Test length(f) == 0
        f = ForwardVector([1,3,3])
        @Test f[2] == 3
        f[2] = 2
        @Test f[2] == 2
        @Test length(f) == 3
        for (i, fi) in enumerate(f)
            @Test i == fi 
        end 

        m = ForwardMatrix([1.0 0.0; 0.0 1.0])
        @Test size(m) == (2,2)
        @Test m[1,1] == 1.0
        m[1,1] = 2.0
        @Test m[1,1] == 2.0
        
        f = ForwardVectorNoLength([1,3,3])
        @test_throws MethodError length(f)
        @Test f[2] == 3
        f[2] = 2
        @Test f[2] == 2
        for (i, fi) in enumerate(f)
            @Test i == fi 
        end 

        l = LockableDict(Dict{String,Int}(), ReentrantLock())
        l["a"] = 1
        @Test l["a"] == 1

        p = ManyProperties("abc", 0, true)
        @Test key1(p) == "abc"
        @Test key2(p) == 0
        @Test key3(p) == true

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

    end
    @testset "@define_interface" begin 
        c = CompositeProperties(SettableProperties("abc", 0, true), 0.0)
        @Test propertynames(c) == (:settable, :key4, :key1, :key2, :key3)
        @Test c.settable == getfield(c, :settable)
        @Test c.key4 == getfield(c, :key4)
        @Test c.key1 == getfield(getfield(c, :settable), :key1)
        @Test c.key2 == getfield(getfield(c, :settable), :key2)
        @Test c.key3 == getfield(getfield(c, :settable), :key3)
        c.key1 = "zzz"
        @Test c.key1 == "zzz"

        c = CompositeProperties2(SettableProperties("abc", 0, true), SettableProperties2(0.0, [0]))
        @Test propertynames(c) == (:settable, :alsosettable, :key1, :key2, :key3, :key4, :key5)
        @Test c.settable == getfield(c, :settable)
        @Test c.alsosettable == getfield(c, :alsosettable)
        @Test c.key1 == getfield(getfield(c, :settable), :key1)
        @Test c.key2 == getfield(getfield(c, :settable), :key2)
        @Test c.key3 == getfield(getfield(c, :settable), :key3)
        @Test c.key4 == getfield(getfield(c, :alsosettable), :key4)
        @Test c.key5 == getfield(getfield(c, :alsosettable), :key5)

        c.key3 = false 
        c.key4 = 1.0
        @Test getfield(getfield(c, :settable), :key3) == false
        @Test getfield(getfield(c, :alsosettable), :key4) == 1.0
        @Test c.alsosettable == SettableProperties2(1.0, [0])
        @Test c.alsosettable != SettableProperties2(1.0, [1])

        e = EqualityUsingProperties(Dict(:a => 1))
        @Test e == EqualityUsingProperties(Dict(:a => 1))
        @Test e != EqualityUsingProperties(Dict(:a => 1, :b => 2))
    end
end