_propertynames(T::Type) = Base.fieldnames(T)
_propertynames(x) = Base.propertynames(x)

@generated function validate_properties(::Type{S}, delegated_fieldnames::Type{T}) where {S, T <: Tuple}
    output = Expr(:block, :(unique_properties = Set{Symbol}($Base.fieldnames($S))))
    recursive = fieldtype(T, 1) === Val{:recursive}
    for i in 2:fieldcount(T)
        key = fieldtype(T, i)
        props = Symbol(key, :_properties)
        child_T = fieldtype(S, key)
        if recursive 
            push!(output.args, :($props = Set{Symbol}( $_propertynames($child_T))))
        else
            push!(output.args, :($props = Set{Symbol}( $fieldnames($child_T))))
        end

        push!( output.args, :( diff = $intersect(unique_properties, $(props)) ), :(!isempty(diff) && error("Duplicate properties `$(sort(collect(diff)))` found for type $($(S)) in child `$($(QuoteNode(key)))::$($(child_T))` ")), :($union!(unique_properties, $props)))
    end
    push!(output.args, :(return nothing))
    return output
end

@generated function _propertynames(::Type{S}, delegated_fields::Type{T}) where {S, T <: Tuple}
    Base.isstructtype(S) || error("$S is not a struct type")
    output = Expr(:tuple, QuoteNode.(fieldnames(S))...)
    recursive = fieldtype(T, 1) === Val{:recursive}
    for i in 2:fieldcount(T)
        key = fieldtype(T,i)
        Si = fieldtype(S, key)
        push!(output.args, recursive ? :($_propertynames($(Si))...) : :($fieldnames($(Si))...))
    end
    return output
end
_propertynames(x, delegated_fields) = _propertynames(typeof(x), delegated_fields)

_hasproperty(T::Type, key::Symbol) = Base.hasfield(T, key)
_hasproperty(x, key::Symbol) = key in _propertynames(x)

_getproperty(x, property::Symbol) = Base.getproperty(x, property)

@generated function _getproperty(x, delegated_fields::Type{T}, property::Symbol) where {T <: Tuple}
    if_else_exprs = Pair{Any,Any}[]
    recursive = fieldtype(T, 1) === Val{:recursive}
    for i in 2:fieldcount(T)
        key = fieldtype(T, i)
        if recursive
            push!( if_else_exprs, :((t = $Base.getfield(x, $(QuoteNode(key))); $_hasproperty(t, property) )) => :( return $Base.getproperty( t, property ) ) )
        else
            push!(if_else_exprs, :( $Base.hasfield($fieldtype($(x), $(QuoteNode(key))), property) ) => :( return $Base.getfield( $Base.getfield(x, $(QuoteNode(key))), property) ))
        end
    end
    else_expr = :(return $Base.getfield(x, property))
    return IfElseExpr(; if_else_exprs, else_expr) |> to_expr
end

@generated function _setproperty!(x, delegated_fields::Type{T}, property::Symbol, v) where {T <: Tuple}
    if_else_exprs = Pair{Any,Any}[]
    recursive = fieldtype(T, 1) === Val{:recursive}
    for i in 2:fieldcount(T)
        key = fieldtype(T, i)
        if recursive
            push!( if_else_exprs, :((t = $Base.getfield(x, $(QuoteNode(key))); $_hasproperty(t, property) )) => :( return $Base.setproperty!( t, property, v ) ) )
        else 
            push!(if_else_exprs, :( $Base.hasfield(fieldtype($(x), $(QuoteNode(key))), property) ) => :( return $Base.setfield!( $Base.getfield(x, $(QuoteNode(key))), property, v) ))
        end
    end
    else_expr = :(return $Base.setfield!(x, property, v))
    return IfElseExpr(; if_else_exprs, else_expr) |> to_expr
end

"""
    properties_interface(T; delegated_fields, [recursive::Bool=false], [ensure_unique::Bool=true])

Provides amalgamated property-based access to `x::T` in terms of its fields as well as the fields of its children. So that, i.e., the fact that `x` is composed of fields `k1::T1, k2::T2, ..., kn::Tn` becomes an implementation detail of the construction of `x`. 

More specifically, given the set of symbols `delegated_fields` and `x::T` for a struct type `T`, defines:

- `Base.propertynames(x::T)` in terms of `fieldnames(T)`, as well as the `fieldnames` of `getfield(x, k)` for each `k` in `delegated_fields`. 
- `Base.getproperty(x::T, name::Symbol)` to return `getfield(getfield(x, k), name)` for the first `k ∈ delegated_fields` such that `hasfield(getfield(x, k), name)`. If no such `k` exists, defaults to returning `getfield(x, name)`.
- and analogously for `Base.setproperty!(x::T, name::Symbol, value)`

If `recursive == true`, instances of `getfield/hasfield/setfield` in the above are replaced by internal methods that default to `Base.getproperty/Base.setproperty/Base.hasproperty`, respectively.

If `ensure_unique == true`, throws an error when there are nonunique names in the amalgamation of the fields of `x` with the fields/recursive properties of each field in `delegated_fields`. If this option is `false`, any of the above setting/getting operations above will use the first (possibly recursive) child field found matching the above criteria. 

# Arguments 
`delegated_fields` can be either a `Symbol`, or a `vect` or `tuple` `Expr` of `Symbol`s, corresponding to fieldnames of `T`

"""
function properties_interface(T; delegated_fields, recursive::Bool=false, ensure_unique::Bool=true, kwargs...)
    if haskey(kwargs, :is_mutable)
        Base.depwarn("Passing `is_mutable` kwarg when `interface=properties` is now deprecated", Symbol("@define_interface"))
        is_mutable = get_kwarg(Bool, kwargs, :is_mutable, false)
    else 
        is_mutable = true
    end
    delegated_fields_sym = parse_vect_of_symbols(delegated_fields; kwarg_name=:delegated_fields)
    delegated_fields_tuple_type = :($Tuple{$Val{$(QuoteNode(recursive ? :recursive : :non_recursive))}, $(QuoteNode.(delegated_fields_sym)...)})
    line_num = current_line_num[]
    linenum! = t-> func_def_line_num!(t, something(line_num, not_provided))

    obj = gensym(:_obj)
    name = gensym(:_name)
    value = gensym(:_value)
    # obj_arg = FuncArg(; name=obj, type=T)
    # name_arg = FuncArg(; name=name, type=:($Base.Symbol))
    # value_arg = FuncArg(; name=value)
    _propertynamesT = :($ForwardMethods._propertynames(::Type{$T}) = $ForwardMethods._propertynames($T, $delegated_fields_tuple_type))
    _propertynames = :($ForwardMethods._propertynames($obj::$T) = $ForwardMethods._propertynames($T))
    propertynames = :($Base.propertynames($obj::$T) = $ForwardMethods._propertynames($obj))
    _getproperty = :($ForwardMethods._getproperty($obj::$T, $name::Symbol) = $ForwardMethods._getproperty($obj, $delegated_fields_tuple_type, $name))
    getproperty = :($Base.getproperty($obj::$T, $name::Symbol) = $ForwardMethods._getproperty($obj, $name))
    _setproperty = :($ForwardMethods._setproperty!($obj::$T, $name::Symbol, $value) = $ForwardMethods._setproperty!($obj, $delegated_fields_tuple_type, $name, $value))
    setproperty = :($Base.setproperty!($obj::$T, $name::Symbol, $value) = $ForwardMethods._setproperty!($obj, $name, $value))


    #_propertynames = FuncDef(; header=FuncCall(; funcname=:($ForwardMethods._propertynames), args=[obj_arg]), head=:(=), line=_sourceinfo, body=:($ForwardMethods._propertynames($obj, $delegated_fields_tuple_type))) |> to_expr
    #_propertynamesT = FuncDef(; header=FuncCall(; funcname=:($ForwardMethods._propertynames), args=[FuncArg(; type=:(Type{$T}))]), head=:(=), line=_sourceinfo, body=:($ForwardMethods._propertynames($T, $delegated_fields_tuple_type))) |> to_expr

#    propertynames = FuncDef(; header=FuncCall(; funcname=:($Base.propertynames), args=[obj_arg]), head=:(=), line=_sourceinfo, body=:($ForwardMethods._propertynames($obj))) |> to_expr
    # _getproperty = FuncDef(; header=FuncCall(; funcname=:($ForwardMethods._getproperty), args=[obj_arg, name_arg]), head=:(=), line=_sourceinfo, body=:($ForwardMethods._getproperty($obj, $delegated_fields_tuple_type, $name))) |> to_expr
    # getproperty = FuncDef(; header=FuncCall(; funcname=:($Base.getproperty), args=[obj_arg, name_arg]), head=:(=), line=_sourceinfo, body=:($ForwardMethods._getproperty($obj, $name))) |> to_expr
    # _setproperty = FuncDef(; header=FuncCall(; funcname=:($ForwardMethods._setproperty!), args=[obj_arg, name_arg, value_arg]), head=:(=), line=_sourceinfo, body=:($ForwardMethods._setproperty!($obj, $delegated_fields_tuple_type, $name, $value))) |> to_expr
    # setproperty = FuncDef(; header=FuncCall(; funcname=:($Base.setproperty!), args=[obj_arg, name_arg, value_arg]), head=:(=), line=_sourceinfo, body=:($ForwardMethods._setproperty!($obj, $name, $value))) |> to_expr

    output = Expr(:block, line_num)
    if ensure_unique
        push!(output.args, :($validate_properties($T, $delegated_fields_tuple_type)))
    end
    push!(output.args, map(linenum!, (_propertynames, _propertynamesT, propertynames, _getproperty, getproperty))...)
    if is_mutable
        push!(output.args, map(linenum!, (_setproperty, setproperty))...)
    end
    return wrap_define_interface(T, :properties, output)
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
        getvalue = :($Base.getfield)
        equal_properties = Expr(:block, :(values = $Base.fieldnames($T)), true)
    elseif compare_fields == :propertynames
        getvalue = :($Base.getproperty)
        equal_properties = Expr(:block, :(values = $Base.propertynames(x)), :(values == $Base.propertynames(y)))
    else
        error("compare_fields (= $compare_fields) must be one of (fieldnames, propertynames)")
    end
    if isempty(omit)
        body = :($Base.all( $equality_op( $getvalue(x, k), $getvalue(y, k) ) for k in values ))
    else
        body = :($Base.all( $equality_op( $getvalue(x, k), $getvalue(y, k) ) for k in values if k ∉ $(Expr(:tuple, QuoteNode.(omit)...))))
    end
    equality_expr = :($Base.$equality_op(x::$T, y::$T) = $equal_properties && $body)
    return wrap_define_interface(T, :equality, equality_expr)
end

const default_define_interfaces = (:properties, :equality, :setfields, :getfields)

for f in default_define_interfaces
    @eval define_interface_method(::Val{$(QuoteNode(f))}) = $(Symbol(f, :_interface))
end

@method_def_constant define_interface_method(::Val{::Symbol}) define_interfaces_available

function define_interface_expr(T, kwargs::Dict{Symbol,Any}=Dict{Symbol,Any}(); _sourceinfo)
    interfaces = interface_kwarg!(kwargs)
    omit = omit_kwarg!(kwargs)
    output = Expr(:block)
    interfaces_available = define_interfaces_available()
    for interface in interfaces
        interface in interfaces_available || error("No interface found with name $interface -- must be one of `$interfaces_available`")
        f = define_interface_method(Val(interface))

        current_line_num[] = _sourceinfo
        try 
            push!(output.args, f(T; omit, kwargs...))
        finally 
            current_line_num[] = nothing 
        end
    end
    return output
end

"""
    @define_interface T [interface=name] [kwargs...]

Defines the `interface` for objects of type `T`

# Arguments 
`name` must be one of $(default_define_interfaces), with `name` value `f` corresponding to the interface definition function `\$f_interface` (e.g., `array` => `array_interface`).

The `key=value` pairs will be forwarded to the corresponding interface definition method. In particular, specifying `omit=func1` or `omit=[func1,func2, ..., funcn]` will omit `func1`, ..., `funcn` from being forwarded by this macro.

Refer to the documentation of each \$(name)_interface for the specific keyword arguments required, if any. 
"""
macro define_interface(T, args...)
    kwargs = Dict{Symbol,Any}()
    for arg in args 
        key, value = parse_kwarg_expr(arg)
        kwargs[key] = value
    end
    return define_interface_expr(T, kwargs; _sourceinfo=__source__) |> esc
end