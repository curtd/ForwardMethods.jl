define_interface_method(x) = nothing

"""
    properties_interface(T; delegated_fields, [is_mutable=false])

Given `x::T` for a struct type `T`, defines `Base.propertynames(x::T)` in terms of the `fieldnames(T)` as well as the fieldnames of `getfield(x, k)` for each `k` in `delegated_fields`.

Also forwards `Base.getproperty(x::T, name::Symbol)` to  `getfield(getfield(x, k), name)` for the first `k` such that `hasfield(getfield(x, k), name)`

If `T` is a mutable type, also defines `Base.setproperty!(x::T, name::Symbol, value)` in a similar fashion to the `Base.getproperty` above. This feature requires at least Julia 1.7. On older Julia versions, pass in the `is_mutable = true` keyword argument.

# Arguments 
`delegated_fields` can be either a `Symbol`, or a `vect` or `tuple` `Expr` of `Symbol`s, corresponding to fieldnames of `T`


"""
function properties_interface(T; delegated_fields, kwargs...)
    delegated_fields_sym = parse_vect_of_symbols(delegated_fields; kwarg_name=:delegated_fields)
    delegated_fields_tuple = Expr(:tuple, QuoteNode.(delegated_fields_sym)...)
    is_mutable = get_kwarg(Bool, kwargs, :is_mutable, false)
    validate_fields = Base.remove_linenums!(quote 
        Base.isstructtype($T) || error("Type "*string($T)*" is not a struct type")

        for field in $delegated_fields_tuple 
            hasfield($T, field) || error("Type "*string($T)*" does not have field :$field")
        end
    end)
    
    properties_body = :(Expr(:tuple, QuoteNode.(all_fields)...))
    gen_getproperty_body = quote 
        local getpropertybody = Expr(:block)
        for (k,v) in pairs(delegated_fieldnames)
            push!(getpropertybody.args, :( if field in $(Expr(:tuple, QuoteNode.(v)...)); return Base.getfield(Base.getfield(x, $(QuoteNode(k))), field) end ))
        end
        push!(getpropertybody.args, :(return Base.getfield(x, field)))
        getpropertybody
    end

    gen_setproperty_body = quote 
        local setpropertybody = Expr(:block)
        for (k,v) in pairs(delegated_fieldnames)
            push!(setpropertybody.args, :( if field in $(Expr(:tuple, QuoteNode.(v)...)); return Base.setproperty!(Base.getfield(x, $(QuoteNode(k))), field, value) end ))
        end
        push!(setpropertybody.args, :(return Base.setproperty!(x, field, value)))
        setpropertybody
    end
        
    output = Expr(:block, validate_fields)
    push!(output.args, quote 
        local fields = fieldnames($T)
        local delegated_fieldnames = $(Expr(:tuple, Expr(:parameters, [Expr(:kw, k, :(Base.fieldnames(Base.fieldtype($T, $(QuoteNode(k)))))) for k in delegated_fields_sym]...)))
        local all_fields = tuple(Iterators.flatten([collect(fields), Iterators.flatten(values(delegated_fieldnames))])...)
        local unique_field_count = Dict{Symbol,Int}()
        for f in all_fields
            unique_field_count[f] = get(unique_field_count, f, 0) + 1
        end
        local duplicate_fields = Symbol[k for (k,v) in pairs(unique_field_count) if v > 1 ]
        isempty(duplicate_fields) || error("Type "* string($T) * " has duplicate field names between itself and/or its requested fields (= $(keys(delegated_fieldnames))) -- duplicate field names = $(Symbol.(sort!(string.(duplicate_fields))))")
        
        local properties_body = $(properties_body)
        local properties_expr = :(Base.propertynames(x::$$T) = $properties_body)
        local getproperty_body = $(gen_getproperty_body)
        local getproperty_expr = :(Base.getproperty(x::$$T, field::Symbol) = $getproperty_body)
        local output = Expr(:block, properties_expr, getproperty_expr)
        if $is_mutable || (VERSION ≥ v"1.7" && Base.ismutabletype($T))
            local setproperty_body = $(gen_setproperty_body)
            local setproperty_expr = :(Base.setproperty!(x::$$T, field::Symbol, value) = $setproperty_body)
            push!(output.args, setproperty_expr)
        end
        eval(output)
    end)
    return output
end

"""
    equality_interface(T; [omit=Symbol[], equality_op=:(==), compare_fields=:fieldnames])

Defines the `equality_op` operator for two objects of type `T` in the natural way, i.e., 
```julia
    Base.:(==)(x::T, y::T) = all( getfield(x,k) == getfield(y,k) for k in fieldnames(T))
```

# Arguments 
If `equality_op == isequal`, defines `Base.isequal` instead.

If `compare_fields == propertynames`, the above definition uses `getproperty` and `propertynames(x)` instead of `getfield` and `fieldnames(T)`, respectively.

Any values provided in `omit` are excluded from the generator expression above.
    
"""
function equality_interface(T; omit::AbstractVector{Symbol}=Symbol[], equality_op::Symbol=:(==), compare_fields::Symbol=:fieldnames)
    equality_op in (:(==), :isequal) || error("equality_op (= $equality_op) must be one of (==, isequal)")
    if compare_fields == :fieldnames 
        getvalue = :(Base.getfield)
        equal_properties = Expr(:block, :(values = fieldnames($T)), true)
    elseif compare_fields == :propertynames
        getvalue = :(Base.getproperty)
        equal_properties = Expr(:block, :(values = Base.propertynames(x)), :(values == Base.propertynames(y)))
    else
        error("compare_fields (= $compare_fields) must be one of (fieldnames, propertynames)")
    end
    if isempty(omit)
        body = :(Base.all( $equality_op( $getvalue(x, k), $getvalue(y, k) ) for k in values ))
    else
        body = :(Base.all( $equality_op( $getvalue(x, k), $getvalue(y, k) ) for k in values if k ∉ $(Expr(:tuple, QuoteNode.(omit)...))))
    end
    equality_expr = :(Base.$equality_op(x::$T, y::$T) = $equal_properties && $body)
    return equality_expr
end

const define_interfaces_available = (:properties, :equality, :setfields, :getfields)

for f in define_interfaces_available
    @eval define_interface_method(::Val{$(QuoteNode(f))}) = $(Symbol(string(f)*"_interface"))
end

function define_interface_expr(T, kwargs::Dict{Symbol,Any}=Dict{Symbol,Any}())
    interfaces = interface_kwarg!(kwargs)
    omit = omit_kwarg!(kwargs)
    output = Expr(:block)
    for interface in interfaces
        f = define_interface_method(Val(interface))
        isnothing(f) && error("No interface found with name $interface -- must be one of `$define_interfaces_available`")
        push!(output.args, f(T; omit, kwargs...))
    end
    return output
end

"""
    @define_interface T [interface=name] [kwargs...]

Defines the `interface` for objects of type `T`

# Arguments 
`name` must be one of $(define_interfaces_available), with `name` value `f` corresponding to the interface definition function `\$f_interface` (e.g., `array` => `array_interface`).

The `key=value` pairs will be forwarded to the corresponding interface definition method. In particular, specifying `omit=func1` or `omit=[func1,func2, ..., funcn]` will omit `func1`, ..., `funcn` from being forwarded by this macro.

Refer to the documentation of each \$(name)_interface for the specific keyword arguments required, if any. 
"""
macro define_interface(T, args...)
    kwargs = Dict{Symbol,Any}()
    for arg in args 
        key, value = parse_kwarg_expr(arg)
        kwargs[key] = value
    end
    return define_interface_expr(T, kwargs) |> esc
end