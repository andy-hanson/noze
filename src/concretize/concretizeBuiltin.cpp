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
			[&](const Bool) {
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
		const ConcreteType t; // the type filling in for ?t in compare comparison(a ?t, b ?t)
		const ConcreteType comparison;
		const ConcreteType less;
		const ConcreteType equal;
		const ConcreteType greater;
	};
	const ComparisonTypes getComparisonTypes(const ConcreteType comparison, const Arr<const ConcreteType> typeArgs) {
		const ConcreteType t = only(typeArgs);
		const Arr<const ConcreteType> unionMembers = comparison.strukt->body().asUnion().members;
		assert(size(unionMembers) == 3);
		const ConcreteType less = at(unionMembers, 0);
		const ConcreteType equal = at(unionMembers, 1);
		const ConcreteType greater = at(unionMembers, 2);
		assert(strEqLiteral(comparison.strukt->mangledName, "comparison")
			&& strEqLiteral(less.strukt->mangledName, "less")
			&& strEqLiteral(equal.strukt->mangledName, "equal")
			&& strEqLiteral(greater.strukt->mangledName, "greater"));
		return ComparisonTypes{t, comparison, less, equal, greater};
	}

	const Opt<const Constant*> tryEvalBuiltinAsConst(
		Arena* arena,
		AllConstants* allConstants,
		const BuiltinFunInfo info,
		const Arr<const ConcreteType> typeArgs,
		const Arr<const Constant*> constArgs,
		const ConcreteType returnType
	) {
		auto typeArg = [&](const size_t index) -> const ConcreteType {
			return at(typeArgs, index);
		};
		auto constantArg = [&](const size_t index) -> const Constant* {
			return at(constArgs, index);
		};
		auto boolArg = [&](const size_t index) -> Bool {
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

		auto constBool = [&](const Bool value) -> const Opt<const Constant*> {
			return yes(constantBool(arena, allConstants, returnType, value));
		};
		auto constInt64 = [&](const Int64 value) -> const Opt<const Constant*> {
			return yes(constantInt64(arena, allConstants, returnType, value));
		};
		auto constNat64 = [&](const Nat64 value) -> const Opt<const Constant*> {
			return yes(constantNat64(arena, allConstants, returnType, value));
		};
		auto constNull = [&](const ConcreteType pointerType) -> const Opt<const Constant*> {
			return yes(constantNull(arena, allConstants, pointerType));
		};
		auto constPtr = [&](const Constant* array, const size_t index) {
			return yes(constantPtr(arena, allConstants, returnType, array, index));
		};
		auto constVoid = [&]() {
			return yes(constantVoid(arena, allConstants, returnType));
		};

		switch (info.kind) {
			case BuiltinFunKind::addFloat64:
				return todo<const Opt<const Constant*>>("concretefunbodyforbuiltin addFloats");

			case BuiltinFunKind::addPtr: {
				const ConstantKind::Ptr ptr = ptrArg(0);
				return constPtr(ptr.array, ptr.index + nat64Arg(1));
			}

			case BuiltinFunKind::_and:
				return constBool(_and(boolArg(0), boolArg(1)));

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
				const Constant* r = constantRecord(arena, allConstants, it.type, emptyArr<const Constant*>());
				return yes(constantUnion(arena, allConstants, types.comparison, it.index, r));
			}

			case BuiltinFunKind::deref:
				return yes(ptrArg(0).deref());

			case BuiltinFunKind::_false:
				return constBool(False);

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
				return constBool(_not(boolArg(0)));

			case BuiltinFunKind::null:
				return constNull(returnType);

			case BuiltinFunKind::oneInt64:
				return constInt64(1);

			case BuiltinFunKind::oneNat64:
				return constNat64(1);

			case BuiltinFunKind::_or:
				return constBool(_or(boolArg(0), boolArg(1)));

			case BuiltinFunKind::pass:
				return constVoid();

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
				return constBool(True);

			case BuiltinFunKind::unsafeDivFloat64:
				return todo<const Opt<const Constant*>>("concretefunbodyforbuiltin divfloats");

			case BuiltinFunKind::unsafeDivInt64:
				return todo<const Opt<const Constant*>>("concretefunbodyforbuiltin divints");

			case BuiltinFunKind::unsafeDivNat64: {
				const Nat64 n0 = nat64Arg(0);
				const Nat64 n1 = nat64Arg(1);

				if (n1 == 0)
					return todo<const Opt<const Constant*>>("unsafe-div failed -- divisor is 0");
				else
					return constNat64(n0 / n1);
			}

			case BuiltinFunKind::unsafeModNat64: {
				const Nat64 n0 = nat64Arg(0);
				const Nat64 n1 = nat64Arg(1);
				if (n1 == 0)
					todo<void>("unsafe-mod failed");
				return constNat64(n0 % n1);
			}

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

			case BuiltinFunKind::wrappingMulNat64:
				return constNat64(nat64Arg(0) * nat64Arg(1));

			case BuiltinFunKind::zeroInt64:
				return constInt64(0);

			case BuiltinFunKind::zeroNat64:
				return constNat64(0);

			default:
				printf("unhandled BuiltinFunKind: %d\n", static_cast<int>(info.kind));
				assert(0);
		}
	}

	template <typename T>
	inline const ConcreteExpr* genExpr(Arena* arena, const ConcreteType type, T value) {
		return nu<const ConcreteExpr>{}(arena, type, SourceRange::empty(), none<const KnownLambdaBody*>(), value);
	}

	const ConcreteExpr* makeLess(
		Arena* arena,
		const ConcreteType boolType,
		const ConcreteExpr* l,
		const ConcreteExpr* r
	) {
		return genExpr(arena, boolType, ConcreteExpr::SpecialBinary{
			ConcreteExpr::SpecialBinary::Kind::less,
			ConstantOrExpr{l},
			ConstantOrExpr{r}});
	}

	const ConcreteExpr* makeCond(
		Arena* arena,
		const ConcreteType type,
		const ConcreteExpr* cond,
		const ConcreteExpr* then,
		const ConcreteExpr* elze
	) {
		return genExpr(arena, type, ConcreteExpr::Cond{cond, ConstantOrExpr{then}, ConstantOrExpr{elze}});
	}

	struct LocalAndExpr {
		const ConcreteLocal* local;
		const ConcreteExpr* expr;
	};

	const LocalAndExpr combineCompares(
		Arena* arena,
		const ConcreteExpr* cmpFirst,
		const ConcreteExpr* cmpSecond,
		const ConcreteType comparisonType
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
		const ConcreteLocal* cmpFirstLocal = nu<const ConcreteLocal>{}(
			arena,
			strLiteral("cmp"),
			comparisonType,
			ConstantOrLambdaOrVariable{ConstantOrLambdaOrVariable::Variable{}});
		const ConcreteExpr* getCmpFirst = genExpr(arena, comparisonType, ConcreteExpr::LocalRef{cmpFirstLocal});
		const ConcreteExpr::Match::Case caseUseFirst = ConcreteExpr::Match::Case{none<const ConcreteLocal*>(), ConstantOrExpr{getCmpFirst}};
		const ConcreteExpr::Match::Case caseUseSecond = ConcreteExpr::Match::Case{none<const ConcreteLocal*>(), ConstantOrExpr{cmpSecond}};
		const Arr<const ConcreteExpr::Match::Case> cases = arrLiteral<const ConcreteExpr::Match::Case>(
			arena,
			{
				/*less*/ caseUseFirst,
				/*equal*/ caseUseSecond,
				/*greater*/ caseUseFirst
			});
		const ConcreteLocal* matchedLocal = nu<const ConcreteLocal>{}(
			arena,
			strLiteral("matched"),
			comparisonType,
			ConstantOrLambdaOrVariable{ConstantOrLambdaOrVariable::Variable{}});
		const ConcreteExpr* then = genExpr(arena, comparisonType, ConcreteExpr::Match{matchedLocal, cmpFirst, cases});
		const ConcreteExpr* res = genExpr(arena, comparisonType, ConcreteExpr::Let{cmpFirstLocal, cmpFirst, ConstantOrExpr{then}});
		return LocalAndExpr{cmpFirstLocal, res};
	}

	const ConcreteFunExprBody generateCompare(ConcretizeCtx* ctx, const ConcreteFunInst concreteFunInst, const ConcreteFun* fun) {
		Arena* arena = ctx->arena;
		const ComparisonTypes types = getComparisonTypes(fun->returnType(), concreteFunInst.typeArgs);

		auto getExpr = [&](const size_t memberIndex, const ConcreteType memberType) -> const ConcreteExpr* {
			const ConcreteExpr* createMember = genExpr(arena, memberType, ConcreteExpr::CreateRecord{emptyArr<const ConstantOrExpr>()});
			return genExpr(arena, types.comparison, ConcreteExpr::ImplicitConvertToUnion{memberIndex, ConstantOrExpr{createMember}});
		};

		const ConcreteExpr* lessExpr = getExpr(0, types.less);
		const ConcreteExpr* equalExpr = getExpr(1, types.equal);
		const ConcreteExpr* greaterExpr = getExpr(2, types.greater);

		assert(fun->arityExcludingCtxAndClosure() == 2);
		const ConcreteParam* aParam = ptrAt(fun->paramsExcludingCtxAndClosure(), 0);
		const ConcreteParam* bParam = ptrAt(fun->paramsExcludingCtxAndClosure(), 1);
		const Bool aIsPointer = aParam->type.isPointer;
		const Bool bIsPointer = bParam->type.isPointer;
		const ConcreteExpr* a = genExpr(arena, types.t, ConcreteExpr::ParamRef{aParam});
		const ConcreteExpr* b = genExpr(arena, types.t, ConcreteExpr::ParamRef{bParam});

		auto getCompareFor = [&](const ConcreteType ct) -> const ConcreteFun* {
			const ConcreteFunKey key = ConcreteFunKey{
				concreteFunInst.withTypeArgs(arrLiteral<const ConcreteType>(arena, { ct })),
				allVariable(arena, fun->arityExcludingCtxAndClosure())};
			return getOrAddConcreteFunAndFillBody(ctx, key);
		};

		const ConcreteType boolType = ctx->boolType();

		if (types.t.isPointer != types.t.strukt->defaultIsPointer())
			todo<void>("compare by value -- just take a ref and compare by ref");

		if (has(types.t.strukt->special))
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
					case BuiltinStructKind::nat64:
					case BuiltinStructKind::ptr: {
						// Output: a < b ? less : b < a ? greater : equal
						const ConcreteExpr* aLessB = makeLess(arena, boolType, a, b);
						const ConcreteExpr* bLessA = makeLess(arena, boolType, b, a);
						const ConcreteExpr* elze = makeCond(arena, types.comparison, bLessA, greaterExpr, equalExpr);
						const ConcreteExpr* expr = makeCond(arena, types.comparison, aLessB, lessExpr, elze);
						return ConcreteFunExprBody{emptyArr<const ConcreteLocal*>(), expr};
					}

					case BuiltinStructKind::funPtrN:
						// should be a compile error? (Or just allow this?)
						return unreachable<const ConcreteFunExprBody>();

					case BuiltinStructKind::_void:
						// should be a constant
						return unreachable<const ConcreteFunExprBody>();

					default:
						assert(0);
				}
			},
			[&](const ConcreteStructBody::Record r) {
				ArrBuilder<const ConcreteLocal*> locals {};
				Cell<const Opt<const ConcreteExpr*>> accum { none<const ConcreteExpr*>() };
				for (const ConcreteField* field : ptrsRange(r.fields)) {
					// for a struct {x, y}, we emit:
					// switch (a.x <=> b.x) { case less: || a.y <=> b.y
					// `||` will short-circuit on any non-zero value.
					// Using special operators `compare` and `orcmp`
					const ConcreteExpr* ax = genExpr(
						arena,
						field->type,
						ConcreteExpr::RecordFieldAccess{aIsPointer, ConstantOrExpr(a), field});
					const ConcreteExpr* bx = genExpr(
						arena,
						field->type,
						ConcreteExpr::RecordFieldAccess{bIsPointer, ConstantOrExpr(b), field});
					const Arr<const ConstantOrExpr> args = arrLiteral<const ConstantOrExpr>(
						arena,
						{ ConstantOrExpr{ax}, ConstantOrExpr{bx} });
					const ConcreteExpr* compareThisField = genExpr(
						arena,
						types.comparison,
						ConcreteExpr::Call{getCompareFor(field->type), args});
					const ConcreteExpr* newAccum = has(cellGet(&accum))
						? [&]() {
							const LocalAndExpr le = combineCompares(
								arena,
								force(cellGet(&accum)),
								compareThisField,
								types.comparison);
							add(arena, &locals, le.local);
							return le.expr;
						}()
						: compareThisField;
					cellSet<const Opt<const ConcreteExpr*>>(&accum, some<const ConcreteExpr*>(newAccum));
				}
				return ConcreteFunExprBody{finishArr(&locals), force(cellGet(&accum))};
			},
			[&](const ConcreteStructBody::Union) {
				return todo<const ConcreteFunExprBody>("compare union");
			},
			[&](const ConcreteStructBody::Iface) {
				return todo<const ConcreteFunExprBody>("compare iface");
			});
	}
}

const ConcreteFunBody getBuiltinFunBody(ConcretizeCtx* ctx, const ConcreteFunSource source, const ConcreteFun* cf) {
	const BuiltinFunInfo info = getBuiltinFunInfo(source.containingFunDecl()->sig);
	const Arr<const ConcreteType> typeArgs = source.typeArgs();
	const Opt<const Arr<const Constant*>> allConstant = tryGetAllConstant(ctx->arena, source.paramsSpecialize);
	if (has(allConstant)) {
		const Opt<const Constant*> c = tryEvalBuiltinAsConst(
			ctx->arena,
			ctx->allConstants,
			info,
			typeArgs,
			force(allConstant),
			cf->returnType());
		return has(c) ? ConcreteFunBody{force(c)} : ConcreteFunBody{ConcreteFunBody::Builtin{info, typeArgs}};
	} else
		switch (info.kind) {
			case BuiltinFunKind::compare:
				return ConcreteFunBody{generateCompare(ctx, source.containingFunInst, cf)};
			default:
				assert(info.emit != BuiltinFunEmit::generate);
				return ConcreteFunBody{ConcreteFunBody::Builtin{info, typeArgs}};
		}
}
