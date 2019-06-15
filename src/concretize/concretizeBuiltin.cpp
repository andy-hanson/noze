#include "./concretizeBuiltin.h"

#include "../util/arrUtil.h"
#include "./builtinInfo.h"
#include "./concretizeUtil.h" // tryGetAllConstant

namespace {
	Comparison constantCompare(const Constant* a, const Constant* b) {
		return a->kind.match(
			[&](const ConstantKind::Array) {
				return todo<const Comparison>("compare arr");
			},
			[&](const bool) {
				return todo<const Comparison>("compare bool");
			},
			[&](const char ca) {
				return comparePrimitive(ca, b->kind.asChar());
			},
			[&](const ConstantKind::FunPtr) {
				// Should be a compile error
				return unreachable<const Comparison>();
			},
			[&](const Int64 ia) {
				return comparePrimitive(ia, b->kind.asInt64());
			},
			[&](const ConstantKind::Lambda) {
				// Should be a compile error
				return unreachable<const Comparison>();
			},
			[&](const Nat64 na) {
				return comparePrimitive(na, b->kind.asNat64());
			},
			[&](const ConstantKind::Null) {
				assert(b->kind.isNull());
				return Comparison::equal;
			},
			[&](const ConstantKind::Ptr) {
				// Should be a compile error
				return unreachable<const Comparison>();
			},
			[&](const ConstantKind::Record) {
				return todo<const Comparison>("compare records");
			},
			[&](const ConstantKind::Union) {
				return todo<const Comparison>("compare union");
			},
			[&](const ConstantKind::Void) {
				assert(b->kind.isVoid());
				return Comparison::equal;
			});
	}

	struct ComparisonTypes {
		const ConcreteType t; // the type parameter
		const ConcreteType comparison;
		const ConcreteType less;
		const ConcreteType equal;
		const ConcreteType greater;
	};
	const ComparisonTypes getComparisonTypes(const ConcreteType comparison, const Arr<const ConcreteType> typeArgs) {
		const ConcreteType t = only(typeArgs);
		const Arr<const ConcreteType> unionMembers = comparison.strukt->body().asUnion().members;
		assert(unionMembers.size == 3);
		const ConcreteType less = unionMembers[0];
		const ConcreteType equal = unionMembers[1];
		const ConcreteType greater = unionMembers[2];
		assert(strEqLiteral(comparison.strukt->mangledName, "comparison")
			&& strEqLiteral(less.strukt->mangledName, "less")
			&& strEqLiteral(equal.strukt->mangledName, "equal")
			&& strEqLiteral(greater.strukt->mangledName, "greater"));
		return ComparisonTypes{t, comparison, less, equal, greater};
	}

	const Opt<const Constant*> tryEvalBuiltinAsConst(
		Arena& arena,
		AllConstants& allConstants,
		const BuiltinFunInfo info,
		const Arr<const ConcreteType> typeArgs,
		const Arr<const Constant*> constArgs,
		const ConcreteType returnType
	) {
		auto typeArg = [&](const size_t index) -> const ConcreteType {
			return typeArgs[index];
		};
		auto constantArg = [&](const size_t index) -> const Constant* {
			return constArgs[index];
		};
		auto boolArg = [&](const size_t index) -> bool {
			return constantArg(index)->kind.asBool();
		};
		auto int64Arg = [&](const size_t index) -> Int64 {
			return constantArg(index)->kind.asInt64();
		};
		auto nat64Arg = [&](const size_t index) -> Nat64 {
			return constantArg(index)->kind.asNat64();
		};
		auto ptrArg = [&](const size_t index) -> const ConstantKind::Ptr {
			return constantArg(index)->kind.asPtr();
		};
		auto yes = [&](const Constant* c) -> const Opt<const Constant*> {
			return some<const Constant*>(c);
		};
		const Opt<const Constant*> no = none<const Constant*>();

		auto constBool = [&](const bool value) -> const Opt<const Constant*> {
			return yes(allConstants._bool(value));
		};
		auto constInt64 = [&](const Int64 value) -> const Opt<const Constant*> {
			return yes(allConstants.int64(arena, value));
		};
		auto constNat64 = [&](const Nat64 value) -> const Opt<const Constant*> {
			return yes(allConstants.nat64(arena, value));
		};
		auto constPtr = [&](const Constant* array, const size_t index) {
			return yes(allConstants.ptr(arena, array, index));
		};
		const Opt<const Constant*> constVoid = yes(allConstants._void);

		switch (info.kind) {
			case BuiltinFunKind::addFloat64:
				return todo<const Opt<const Constant*>>("concretefunbodyforbuiltin addFloats");

			case BuiltinFunKind::addPtr: {
				const ConstantKind::Ptr ptr = ptrArg(0);
				return constPtr(ptr.array, ptr.index + nat64Arg(1));
			}

			case BuiltinFunKind::_and:
				return constBool(boolArg(0) && boolArg(1));

			case BuiltinFunKind::as:
				return yes(constantArg(0));

			// Was marked as non-specializable
			case BuiltinFunKind::asNonConst:
			// 'concretizeCall' handles this specially when the lambda is known.
			case BuiltinFunKind::callFunPtr:
				return unreachable<const Opt<const Constant*>>();

			case BuiltinFunKind::compare: {
				const Comparison cmp = constantCompare(constantArg(0), constantArg(1));
				const ComparisonTypes types = getComparisonTypes(returnType, typeArgs);
				struct IndexAndType {
					const size_t index;
					const ConcreteType type;
				};
				const IndexAndType it = [&]() {
					switch (cmp) {
						case Comparison::less:
							return IndexAndType{0, types.less};
						case Comparison::equal:
							return IndexAndType{1, types.equal};
						case Comparison::greater:
							return IndexAndType{2, types.greater};
						default:
							assert(0);
					}
				}();
				const Constant* r = allConstants.record(arena, it.type, emptyArr<const Constant*>());
				return yes(allConstants._union(arena, types.comparison, it.index, r));
			}

			case BuiltinFunKind::deref:
				return yes(ptrArg(0).deref());

			case BuiltinFunKind::_false:
				return constBool(false);

			case BuiltinFunKind::getCtx:
				return no;

			case BuiltinFunKind::hardFail:
				return no;

			case BuiltinFunKind::_if:
				return yes(boolArg(0) ? constantArg(1) : constantArg(2));

			case BuiltinFunKind::isReferenceType:
				return constBool(typeArg(0).isPointer);

			case BuiltinFunKind::mulFloat64:
				return todo<const Opt<const Constant*>>("concretefunbodyforbuiltin mulfloats");

			case BuiltinFunKind::_not:
				return constBool(!boolArg(0));

			case BuiltinFunKind::oneInt64:
				return constInt64(1);

			case BuiltinFunKind::oneNat64:
				return constNat64(1);

			case BuiltinFunKind::_or:
				return constBool(boolArg(0) || boolArg(1));

			case BuiltinFunKind::pass:
				return constVoid;

			case BuiltinFunKind::ptrCast:
				return no;

			case BuiltinFunKind::refOfVal:
				return no; // TODO: could be a const

			case BuiltinFunKind::setPtr:
				return no;

			case BuiltinFunKind::sizeOf: {
				const ConcreteType t = typeArg(0);
				if (t.isPointer != t.strukt->defaultIsPointer())
					todo<void>("is this doing `sizeof<byval<?t>>` ?");
				return constNat64(t.strukt->sizeBytes());
			}

			case BuiltinFunKind::subFloat64:
				return todo<const Opt<const Constant*>>("concretefunbodyforbuiltin subfloats");

			case BuiltinFunKind::_true:
				return constBool(true);

			case BuiltinFunKind::unsafeDivFloat64:
				return todo<const Opt<const Constant*>>("concretefunbodyforbuiltin divfloats");
			case BuiltinFunKind::unsafeDivInt64:
				return todo<const Opt<const Constant*>>("concretefunbodyforbuiltin divints");
			case BuiltinFunKind::unsafeDivNat64:
				return todo<const Opt<const Constant*>>("concretefunbodyforbuiltin divnats");
			case BuiltinFunKind::wrappingAddInt64: {
				const Int64 i0 = int64Arg(0);
				const Int64 i1 = int64Arg(1);
				if (i0 < -9999 || i0 > 9999 || i1 < -9999 || i1 > 9999)
					todo<void>("c++ doesn't use wrapping addition for signed ints, must emulate");
				return constInt64(i0 + i1);
			}
			case BuiltinFunKind::wrappingAddNat64:
				return constNat64(nat64Arg(0) + nat64Arg(1));

			case BuiltinFunKind::wrappingSubInt64: {
				const Int64 i0 = int64Arg(0);
				const Int64 i1 = int64Arg(1);
				if (i0 < -9999 || i0 > 9999 || i1 < -9999 || i1 > 9999)
					todo<void>("c++ doesn't use wrapping addition for signed ints, must emulate");
				return constInt64(i0 + i1);
			}

			case BuiltinFunKind::wrappingSubNat64:
				return constNat64(nat64Arg(0) - nat64Arg(1));

			case BuiltinFunKind::wrappingMulInt64: {
				const Int64 i0 = int64Arg(0);
				const Int64 i1 = int64Arg(1);
				if (i0 < -9999 || i0 > 9999 || i1 < -9999 || i1 > 9999)
					todo<void>("c++ doesn't use wrapping addition for signed ints, must emulate");
				return constInt64(i0 * i1);
			}

			case BuiltinFunKind::zeroInt64:
				return constInt64(0);

			case BuiltinFunKind::zeroNat64:
				return constNat64(0);

			default:
				assert(0);
		}
	}

	template <typename T>
	inline const ConcreteExpr* genExpr(Arena& arena, T value) {
		return arena.nu<const ConcreteExpr>()(SourceRange::empty(), none<const KnownLambdaBody*>(), value);
	}

	const ConcreteExpr* makeLess(Arena& arena, const ConcreteExpr* l, const ConcreteExpr* r) {
		return genExpr(arena, ConcreteExpr::SpecialBinary{
			ConcreteExpr::SpecialBinary::Kind::less,
			ConstantOrExpr{l},
			ConstantOrExpr{r}});
	}

	const ConcreteExpr* makeCond(Arena& arena, const ConcreteExpr* cond, const ConcreteExpr* then, const ConcreteExpr* elze) {
		return genExpr(arena, ConcreteExpr::Cond{cond, ConstantOrExpr{then}, ConstantOrExpr{elze}});
	}

	const ConcreteExpr* combineCompares(
		Arena& arena,
		const ConcreteExpr* cmpFirst,
		const ConcreteExpr* cmpSecond,
		const ConcreteType comparisonStruct
	) {
		/*
		Outputs:
		Comparison cmp = <<cmpFirst>>;
		switch (cmp.kind) {
			case Comparison.Kind.less:
				return cmp;
			case Comparison.Kind.equal:
				<<cmpSecond>>
			case Comparison.Kind.greater:
				return cmp;
		}
		*/
		const ConcreteLocal* cmpFirstLocal = arena.nu<const ConcreteLocal>()(
			strLiteral("cmp"),
			comparisonStruct,
			ConstantOrLambdaOrVariable{ConstantOrLambdaOrVariable::Variable{}});
		const ConcreteExpr* getCmpFirst = genExpr(arena, ConcreteExpr::LocalRef{cmpFirstLocal});
		const ConcreteExpr::Match::Case caseUseFirst = ConcreteExpr::Match::Case{none<const ConcreteLocal*>(), ConstantOrExpr{getCmpFirst}};
		const ConcreteExpr::Match::Case caseUseSecond = ConcreteExpr::Match::Case{none<const ConcreteLocal*>(), ConstantOrExpr{cmpSecond}};
		const Arr<const ConcreteExpr::Match::Case> cases = arrLiteral<const ConcreteExpr::Match::Case>(
			arena,
			/*less*/ caseUseFirst,
			/*equal*/ caseUseSecond,
			/*greater*/ caseUseFirst);
		const ConcreteExpr* then = genExpr(arena, ConcreteExpr::Match{cmpFirst, comparisonStruct.mustBeNonPointer(), cases});
		return genExpr(arena, ConcreteExpr::Let{cmpFirstLocal, cmpFirst, ConstantOrExpr{then}});
	}

	const ConcreteExpr* generateCompare(ConcretizeCtx& ctx, const FunDeclAndTypeArgs declAndTypeArgs, const ConcreteFun* fun) {
		Arena& arena = ctx.arena;
		const ComparisonTypes types = getComparisonTypes(fun->returnType(), declAndTypeArgs.typeArgs);

		auto getExpr = [&](const ConcreteType memberType) -> const ConcreteExpr* {
			const ConcreteExpr* createMember = genExpr(arena, ConcreteExpr::CreateRecord{memberType, none<const ConcreteFun*>(), emptyArr<const ConstantOrExpr>()});
			return genExpr(arena, ConcreteExpr::ImplicitConvertToUnion{types.comparison, memberType, ConstantOrExpr{createMember}});
		};

		const ConcreteExpr* lessExpr = getExpr(types.less);
		const ConcreteExpr* equalExpr = getExpr(types.equal);
		const ConcreteExpr* greaterExpr = getExpr(types.greater);

		assert(fun->arity() == 2);
		const ConcreteExpr* a = genExpr(arena, ConcreteExpr::ParamRef{fun->paramsExcludingClosure().getPtr(0)});
		const ConcreteExpr* b = genExpr(arena, ConcreteExpr::ParamRef{fun->paramsExcludingClosure().getPtr(1)});

		auto getCompareFor = [&](const ConcreteType ct) -> const ConcreteFun* {
			const ConcreteFunKey key = ConcreteFunKey{
				FunDeclAndTypeArgsAndSpecImpls{
					declAndTypeArgs.withTypeArgs(arrLiteral<const ConcreteType>(arena, ct)),
					emptyArr<const FunDecl*>(),
				},
				allVariable(arena, fun->arity())};
			return getOrAddConcreteFunAndFillBody(ctx, key);
		};

		if (types.t.isPointer != types.t.strukt->defaultIsPointer())
			todo<void>("compare by value -- just take a ref and compare by ref");

		if (types.t.strukt->special.has())
			todo<void>("compare special");

		return types.t.strukt->body().match(
			[&](const ConcreteStructBody::Builtin builtin) {
				const BuiltinStructInfo info = builtin.info;
				switch (info.kind) {
					case BuiltinStructKind::_bool:
					case BuiltinStructKind::byte:
					case BuiltinStructKind::_char:
					case BuiltinStructKind::float64:
					case BuiltinStructKind::int64:
					case BuiltinStructKind::nat64: {
						// Output: a < b ? less : b < a ? greater : equal
						const ConcreteExpr* aLessB = makeLess(arena, a, b);
						const ConcreteExpr* bLessA = makeLess(arena, b, a);
						return makeCond(arena, aLessB, lessExpr, makeCond(arena, bLessA, greaterExpr, equalExpr));
					}

					case BuiltinStructKind::funPtrN:
					case BuiltinStructKind::ptr:
						// should be a compile error
						return unreachable<const ConcreteExpr*>();

					case BuiltinStructKind::_void:
						// should be a constant
						return unreachable<const ConcreteExpr*>();

					default:
						assert(0);
				}
			},
			[&](const ConcreteStructBody::Fields fields) {
				Cell<const Opt<const ConcreteExpr*>> accum { none<const ConcreteExpr*>() };
				for (const ConcreteField* field : ptrsRange(fields.fields)) {
					// for a struct {x, y}, we emit:
					// switch (a.x <=> b.x) { case less: || a.y <=> b.y
					// `||` will short-circuit on any non-zero value.
					// Using special operators `compare` and `orcmp`
					const ConcreteExpr* ax = genExpr(arena, ConcreteExpr::StructFieldAccess{ConstantOrExpr(a), field});
					const ConcreteExpr* bx = genExpr(arena, ConcreteExpr::StructFieldAccess{ConstantOrExpr(b), field});
					const Arr<const ConstantOrExpr> args = arrLiteral<const ConstantOrExpr>(arena, ConstantOrExpr{ax}, ConstantOrExpr{bx});
					const ConcreteExpr* compareThisField = genExpr(arena, ConcreteExpr::CallConcreteFun{getCompareFor(field->type), args});
					const ConcreteExpr* newAccum = accum.get().has()
						? combineCompares(arena, accum.get().force(), compareThisField, types.comparison)
						: compareThisField;
					accum.set(some<const ConcreteExpr*>(newAccum));
				}
				return accum.get().force();
			},
			[&](const ConcreteStructBody::Union) {
				return todo<const ConcreteExpr*>("compare union");
			},
			[&](const ConcreteStructBody::Iface) {
				return todo<const ConcreteExpr*>("compare iface");
			});
	}
}

const ConcreteFunBody getBuiltinFunBody(ConcretizeCtx& ctx, const ConcreteFunSource source, const ConcreteFun* cf) {
	const BuiltinFunInfo info = getBuiltinFunInfo(source.containingFunDecl()->sig);
	const Arr<const ConcreteType> typeArgs = source.typeArgs();
	const Opt<const Arr<const Constant*>> allConstant = tryGetAllConstant(ctx.arena, source.paramsSpecialize);
	if (allConstant.has()) {
		const Opt<const Constant*> c = tryEvalBuiltinAsConst(
			ctx.arena,
			ctx.allConstants,
			info,
			typeArgs,
			allConstant.force(),
			cf->returnType());
		return c.has() ? ConcreteFunBody{c.force()} : ConcreteFunBody{ConcreteFunBody::Builtin{info, typeArgs}};
	} else
		switch (info.kind) {
			case BuiltinFunKind::compare:
				return ConcreteFunBody{generateCompare(ctx, source.containingFunDeclAndTypeArgs(), cf)};
			default:
				assert(info.emit != BuiltinFunEmit::generate);
				return ConcreteFunBody{ConcreteFunBody::Builtin{info, typeArgs}};
		}
}
