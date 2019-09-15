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
			[&](const Int16 ia) {
				return compareInt16(ia, b->kind.asInt16());
			},
			[&](const Int32 ia) {
				return compareInt32(ia, b->kind.asInt32());
			},
			[&](const Int64 ia) {
				return compareInt64(ia, b->kind.asInt64());
			},
			[&](const ConstantKind::Lambda) {
				// Should be a compile error
				return unreachable<const Comparison>();
			},
			[&](const Nat16 na) {
				return compareNat16(na, b->kind.asNat16());
			},
			[&](const Nat32 na) {
				return compareNat32(na, b->kind.asNat32());
			},
			[&](const Nat64 na) {
				return compareNat64(na, b->kind.asNat64());
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
		auto int16Arg = [&](const size_t index) -> Int16 {
			return constantArg(index)->kind.asInt16();
		};
		auto int32Arg = [&](const size_t index) -> Int32 {
			return constantArg(index)->kind.asInt32();
		};
		auto int64Arg = [&](const size_t index) -> Int64 {
			return constantArg(index)->kind.asInt64();
		};
		auto nat16Arg = [&](const size_t index) -> Nat16 {
			return constantArg(index)->kind.asNat16();
		};
		auto nat32Arg = [&](const size_t index) -> Nat32 {
			return constantArg(index)->kind.asNat32();
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
		auto constInt16 = [&](const Int16 value) -> const Opt<const Constant*> {
			return yes(constantInt16(arena, allConstants, returnType, value));
		};
		auto constInt32 = [&](const Int32 value) -> const Opt<const Constant*> {
			return yes(constantInt32(arena, allConstants, returnType, value));
		};
		auto constInt64 = [&](const Int64 value) -> const Opt<const Constant*> {
			return yes(constantInt64(arena, allConstants, returnType, value));
		};
		auto constNat16 = [&](const Nat16 value) -> const Opt<const Constant*> {
			return yes(constantNat16(arena, allConstants, returnType, value));
		};
		auto constNat32 = [&](const Nat32 value) -> const Opt<const Constant*> {
			return yes(constantNat32(arena, allConstants, returnType, value));
		};
		auto constNat64 = [&](const Nat64 value) -> const Opt<const Constant*> {
			return yes(constantNat64(arena, allConstants, returnType, value));
		};
		auto constNull = [&](const ConcreteType pointerType) -> const Opt<const Constant*> {
			return yes(constantNull(arena, allConstants, pointerType));
		};
		auto constPtr = [&](const Constant* array, const Nat64 index) {
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
			case BuiltinFunKind::getErrno:
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

			case BuiltinFunKind::oneInt16:
				return constInt16(Int16{1});

			case BuiltinFunKind::oneInt32:
				return constInt32(Int32{1});

			case BuiltinFunKind::oneInt64:
				return constInt64(Int64{1});

			case BuiltinFunKind::oneNat64:
				return constNat64(Nat64{1});

			case BuiltinFunKind::oneNat32:
				return constNat32(Nat32{1});

			case BuiltinFunKind::oneNat16:
				return constNat16(Nat16{1});

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

			case BuiltinFunKind::sizeOf:
				return constNat64(Nat64{sizeOrPointerSizeBytes(typeArg(0))});

			case BuiltinFunKind::subFloat64:
				return todo<const Opt<const Constant*>>("concretefunbodyforbuiltin subfloats");

			case BuiltinFunKind::toNatFromNat32:
				return constNat64(toNat64(nat32Arg(0)));

			case BuiltinFunKind::_true:
				return constBool(True);

			case BuiltinFunKind::unsafeDivFloat64:
				return todo<const Opt<const Constant*>>("concretefunbodyforbuiltin divfloats");

			case BuiltinFunKind::unsafeDivInt64:
				return todo<const Opt<const Constant*>>("concretefunbodyforbuiltin divints");

			case BuiltinFunKind::unsafeDivNat64: {
				const Nat64 n0 = nat64Arg(0);
				const Nat64 n1 = nat64Arg(1);

				if (n1.value == 0)
					return todo<const Opt<const Constant*>>("unsafe-div failed -- divisor is 0");
				else
					return constNat64(n0 / n1);
			}

			case BuiltinFunKind::unsafeModNat64: {
				const Nat64 n0 = nat64Arg(0);
				const Nat64 n1 = nat64Arg(1);
				if (n1.value == 0)
					todo<void>("unsafe-mod failed");
				return constNat64(n0 % n1);
			}

			case BuiltinFunKind::unsafeNat64ToInt64:
				return constInt64(int64FromNat64(nat64Arg(0)));

			case BuiltinFunKind::unsafeNat64ToNat32:
				return constNat32(nat32FromNat64(nat64Arg(0)));

			case BuiltinFunKind::unsafeInt64ToNat64:
				return todo<const Opt<const Constant*>>("unsafeInt64ToNat64");

			case BuiltinFunKind::wrapAddInt16:
				return constInt16(wrapAdd(int16Arg(0), int16Arg(1)));

			case BuiltinFunKind::wrapAddInt32:
				return constInt32(wrapAdd(int32Arg(0), int32Arg(1)));

			case BuiltinFunKind::wrapAddInt64:
				return constInt64(wrapAdd(int64Arg(0), int64Arg(1)));

			case BuiltinFunKind::wrapAddNat16:
				return constNat16(nat16Arg(0) + nat16Arg(1));

			case BuiltinFunKind::wrapAddNat32:
				return constNat32(nat32Arg(0) + nat32Arg(1));

			case BuiltinFunKind::wrapAddNat64:
				return constNat64(nat64Arg(0) + nat64Arg(1));

			case BuiltinFunKind::wrapSubInt16:
				return constInt16(wrapSub(int16Arg(0), int16Arg(1)));

			case BuiltinFunKind::wrapSubInt32:
				return constInt32(wrapSub(int32Arg(0), int32Arg(1)));

			case BuiltinFunKind::wrapSubInt64:
				return constInt64(wrapSub(int64Arg(0), int64Arg(1)));

			case BuiltinFunKind::wrapSubNat32:
				return constNat32(nat32Arg(0) - nat32Arg(1));

			case BuiltinFunKind::wrapSubNat64:
				return constNat64(nat64Arg(0) - nat64Arg(1));

			case BuiltinFunKind::wrapMulInt64: {
				const Int64 i0 = int64Arg(0);
				const Int64 i1 = int64Arg(1);
				if (i0.value < -9999999 || i0.value > 9999999 || i1.value < -9999999 || i1.value > 9999999)
					todo<void>("c++ doesn't use wrapping addition for signed ints, must emulate");
				return constInt64(i0 * i1);
			}

			case BuiltinFunKind::wrapMulNat32:
				return constNat32(nat32Arg(0) * nat32Arg(1));
			case BuiltinFunKind::wrapMulNat64:
				return constNat64(nat64Arg(0) * nat64Arg(1));

			case BuiltinFunKind::zeroInt16:
				return constInt16(Int16{0});

			case BuiltinFunKind::zeroInt32:
				return constInt32(Int32{0});

			case BuiltinFunKind::zeroInt64:
				return constInt64(Int64{0});

			case BuiltinFunKind::zeroNat16:
				return constNat16(Nat16{0});

			case BuiltinFunKind::zeroNat32:
				return constNat32(Nat32{0});

			case BuiltinFunKind::zeroNat64:
				return constNat64(Nat64{0});

			default:
				printf("concretizeBuiltin: unhandled BuiltinFunKind: %d\n", static_cast<int>(info.kind));
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

	struct LocalsAndExpr {
		const Arr<const ConcreteLocal*> locals;
		const ConcreteExpr* expr;
	};

	const LocalsAndExpr combineCompares(
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
		const ConcreteExpr::Match::Case caseUseFirst =
			ConcreteExpr::Match::Case{none<const ConcreteLocal*>(), ConstantOrExpr{getCmpFirst}};
		const ConcreteExpr::Match::Case caseUseSecond =
			ConcreteExpr::Match::Case{none<const ConcreteLocal*>(), ConstantOrExpr{cmpSecond}};
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
		const ConcreteExpr* res =
			genExpr(arena, comparisonType, ConcreteExpr::Let{cmpFirstLocal, cmpFirst, ConstantOrExpr{then}});
		return LocalsAndExpr{
			arrLiteral<const ConcreteLocal*>(arena, { cmpFirstLocal, matchedLocal }),
			res};
	}

	const ConcreteExpr* genFieldAccess(
		Arena* arena,
		const ConcreteExpr* a,
		const Bool aIsPointer,
		const ConcreteField* field
	) {
		return genExpr(arena, field->type, ConcreteExpr::RecordFieldAccess{aIsPointer, ConstantOrExpr{a}, field});
	}

	const ConcreteExpr* genCreateArr(
		Arena* arena,
		const ConcreteType arrType,
		const ConstantOrExpr size,
		const ConstantOrExpr data
	) {
		const Arr<const ConstantOrExpr> args = arrLiteral<const ConstantOrExpr>(arena, { size, data });
		return genExpr(arena, arrType, ConcreteExpr::CreateRecord{args});
	}

	const ConcreteFun* getCompareFunFor(
		ConcretizeCtx* ctx,
		const ConcreteFunInst compareFunInst,
		const ConcreteType ct
	) {
		const ConcreteFunKey key = ConcreteFunKey{
			compareFunInst.withTypeArgs(arrLiteral<const ConcreteType>(ctx->arena, { ct })),
			allVariable(ctx->arena, 2)};
		return getOrAddConcreteFunAndFillBody(ctx, key);
	};

	const ConcreteExpr* genCall(Arena* arena, const ConcreteFun* fun, const Arr<const ConstantOrExpr> args) {
		return genExpr(arena, fun->returnType(), ConcreteExpr::Call{fun, args});
	}

	const ConcreteExpr* genCall(
		Arena* arena,
		const ConcreteFun* fun,
		const ConstantOrExpr arg0,
		const ConstantOrExpr arg1
	) {
		return genCall(arena, fun, arrLiteral<const ConstantOrExpr>(arena, { arg0, arg1 }));
	}

	const ConcreteExpr* genCompare(
		ConcretizeCtx* ctx,
		const ConcreteFunInst compareFunInst,
		const ConcreteType argsType,
		const ConstantOrExpr l,
		const ConstantOrExpr r
	) {
		return genCall(
			ctx->arena,
			getCompareFunFor(ctx, compareFunInst, argsType),
			ConstantOrExpr{l},
			ConstantOrExpr{r});
	}

	const ConcreteExpr* compareOneField(
		ConcretizeCtx* ctx,
		const ConcreteFunInst compareFunInst,
		const ConcreteField* field,
		const ConcreteExpr* a,
		const ConcreteExpr* b,
		const Bool aIsPointer,
		const Bool bIsPointer
	) {
		const ConcreteExpr* ax = genFieldAccess(ctx->arena, a, aIsPointer, field);
		const ConcreteExpr* bx = genFieldAccess(ctx->arena, b, bIsPointer, field);
		return genCompare(ctx, compareFunInst, field->type, ConstantOrExpr{ax}, ConstantOrExpr{bx});
	}

	const ConcreteFunExprBody generateCompareRecord(
		ConcretizeCtx* ctx,
		const ConcreteFunInst concreteFunInst,
		const ComparisonTypes types,
		const ConcreteStructBody::Record r,
		const ConcreteExpr* a,
		const ConcreteExpr* b,
		const Bool aIsPointer,
		const Bool bIsPointer
	) {
		Arena* arena = ctx->arena;

		ArrBuilder<const ConcreteLocal*> locals {};
		Cell<const Opt<const ConcreteExpr*>> accum { none<const ConcreteExpr*>() };
		for (const ConcreteField* field : ptrsRange(r.fields)) {
			const ConcreteExpr* compareThisField =
				compareOneField(ctx, concreteFunInst, field, a, b, aIsPointer, bIsPointer);
			const ConcreteExpr* newAccum = has(cellGet(&accum))
				? [&]() {
					const LocalsAndExpr le = combineCompares(
						arena,
						force(cellGet(&accum)),
						compareThisField,
						types.comparison);
					addMany<const ConcreteLocal*>(arena, &locals, le.locals);
					return le.expr;
				}()
				: compareThisField;
			cellSet<const Opt<const ConcreteExpr*>>(&accum, some<const ConcreteExpr*>(newAccum));
		}
		return ConcreteFunExprBody{finishArr(&locals), force(cellGet(&accum))};
	}

	const ConcreteExpr* genDecrNat(ConcretizeCtx* ctx, const ConcreteType natType, const ConcreteExpr* n) {
		return genExpr(
			ctx->arena,
			natType,
			ConcreteExpr::SpecialBinary{
				ConcreteExpr::SpecialBinary::Kind::sub,
				ConstantOrExpr{n},
				ConstantOrExpr{constantNat64(ctx->arena, ctx->allConstants, natType, Nat64{1})}});
	}

	const ConcreteExpr* genNatEqNat(
		Arena* arena,
		const ConcreteType boolType,
		const ConstantOrExpr a,
		const ConstantOrExpr b
	) {
		return genExpr(arena, boolType, ConcreteExpr::SpecialBinary{ConcreteExpr::SpecialBinary::Kind::eq, a, b});
	}

	const ConcreteExpr* genNatEqZero(
		ConcretizeCtx* ctx,
		const ConcreteType boolType,
		const ConcreteType natType,
		const ConcreteExpr* n
	) {
		return genNatEqNat(
			ctx->arena,
			boolType,
			ConstantOrExpr{n},
			ConstantOrExpr{constantNat64(ctx->arena, ctx->allConstants, natType, Nat64{0})});
	}

	const ConcreteExpr* genIncrPointer(
		ConcretizeCtx* ctx,
		const ConcreteType ptrType,
		const ConcreteType natType,
		const ConcreteExpr* ptr
	) {
		return genExpr(
			ctx->arena,
			ptrType,
			ConcreteExpr::SpecialBinary{
				ConcreteExpr::SpecialBinary::Kind::add,
				ConstantOrExpr{ptr},
				ConstantOrExpr{constantNat64(ctx->arena, ctx->allConstants, natType, Nat64{1})}});
	}

	const ConcreteExpr* genDeref(
		Arena* arena,
		const ConcreteType derefedType,
		const ConcreteExpr* ptr
	) {
		return genExpr(
			arena,
			derefedType,
			ConcreteExpr::SpecialUnary{ConcreteExpr::SpecialUnary::Kind::deref, ptr});
	}

	const ConcreteExpr* genLtEqOrGtHelper(
		Arena* arena,
		const ComparisonTypes types,
		const size_t memberIndex,
		const ConcreteType memberType
	) {
		const ConcreteExpr* createMember =
			genExpr(arena, memberType, ConcreteExpr::CreateRecord{emptyArr<const ConstantOrExpr>()});
		return genExpr(
			arena,
			types.comparison,
			ConcreteExpr::ImplicitConvertToUnion{memberIndex, ConstantOrExpr{createMember}});
	}
	const ConcreteExpr* genLessLiteral(Arena* arena, const ComparisonTypes types) {
		return genLtEqOrGtHelper(arena, types, 0, types.less);
	}
	const ConcreteExpr* genEqualLiteral(Arena* arena, const ComparisonTypes types) {
		return genLtEqOrGtHelper(arena, types, 1, types.equal);
	}
	const ConcreteExpr* genGreaterLiteral(Arena* arena, const ComparisonTypes types) {
		return genLtEqOrGtHelper(arena, types, 2, types.greater);
	}

	const ConcreteFunExprBody generateCompareArr(
		ConcretizeCtx* ctx,
		const ConcreteFunInst compareFunInst,
		const ConcreteType comparisonType,
		const ConcreteFun* compareFun,
		const ComparisonTypes types,
		const ConcreteType elementType,
		const ConcreteExpr* a,
		const ConcreteExpr* b
	) {
		// a.size == 0 ? (b.size == 0 ? eq : lt) : (b.size == 0 ? gt : (a[0] == b[0] or cmp(tail(a), tail(b))))
		const ConcreteType arrType = first(compareFun->paramsExcludingCtxAndClosure()).type;

		Arena* arena = ctx->arena;
		assert(!arrType.isPointer);
		const ConcreteStructBody::Record r = arrType.strukt->body().asRecord();
		assert(size(r.fields) == 2);
		const ConcreteField* sizeField = ptrAt(r.fields, 0);
		const ConcreteField* dataField = ptrAt(r.fields, 1);
		assert(strEq(sizeField->mangledName, strLiteral("size")));
		assert(strEq(dataField->mangledName, strLiteral("data")));

		const ConcreteType boolType = ctx->boolType();
		const ConcreteType natType = sizeField->type;
		const ConcreteType ptrType = dataField->type;

		auto genGetSize = [&](const ConcreteExpr* arr) -> const ConcreteExpr* {
			return genFieldAccess(arena, arr, False, sizeField);
		};
		auto genGetData = [&](const ConcreteExpr* arr) -> const ConcreteExpr* {
			return genFieldAccess(arena, arr, False, dataField);
		};

		auto genTail = [&](const ConcreteExpr* arr) -> const ConstantOrExpr {
			const ConcreteExpr* curSize = genGetSize(arr);
			const ConcreteExpr* curData = genGetData(arr);
			const ConcreteExpr* newSize = genDecrNat(ctx, natType, curSize);
			const ConcreteExpr* newData = genIncrPointer(ctx, ptrType, natType, curData);
			return ConstantOrExpr{
				genCreateArr(arena, arrType, ConstantOrExpr{newSize}, ConstantOrExpr{newData})};
		};

		auto genFirst = [&](const ConcreteExpr* arr) -> const ConstantOrExpr {
			return ConstantOrExpr{genDeref(arena, elementType, genGetData(arr))};
		};

		const ConcreteExpr* compareFirst = genCompare(
			ctx,
			compareFunInst,
			elementType,
			genFirst(a),
			genFirst(b));
		const ConcreteExpr* recurOnTail = genCall(arena, compareFun, genTail(a), genTail(b));
		const LocalsAndExpr firstThenRecur = combineCompares(arena, compareFirst, recurOnTail, types.comparison);

		auto genSizeEqZero = [&](const ConcreteExpr* arr) -> const ConcreteExpr* {
			return genNatEqZero(ctx, boolType, natType, genGetSize(arr));
		};

		const ConcreteExpr* bSizeIsZero = genSizeEqZero(b);
		const ConcreteExpr* res = makeCond(
			arena,
			comparisonType,
			genSizeEqZero(a),
			makeCond(arena, comparisonType, bSizeIsZero, genEqualLiteral(arena, types), genLessLiteral(arena, types)),
			makeCond(arena, comparisonType, bSizeIsZero, genGreaterLiteral(arena, types), firstThenRecur.expr));

		return ConcreteFunExprBody{firstThenRecur.locals, res};
	}


	const ConcreteFunExprBody generateCompare(
		ConcretizeCtx* ctx,
		const ConcreteFunInst compareFunInst,
		const ConcreteFun* compareFun
	) {
		Arena* arena = ctx->arena;
		const ComparisonTypes types = getComparisonTypes(compareFun->returnType(), compareFunInst.typeArgs);
		assert(compareFun->arityExcludingCtxAndClosure() == 2);
		const ConcreteParam* aParam = ptrAt(compareFun->paramsExcludingCtxAndClosure(), 0);
		const ConcreteParam* bParam = ptrAt(compareFun->paramsExcludingCtxAndClosure(), 1);
		const Bool aIsPointer = aParam->type.isPointer;
		const Bool bIsPointer = bParam->type.isPointer;
		const ConcreteExpr* a = genExpr(arena, types.t, ConcreteExpr::ParamRef{aParam});
		const ConcreteExpr* b = genExpr(arena, types.t, ConcreteExpr::ParamRef{bParam});

		const ConcreteType boolType = ctx->boolType();

		if (types.t.isPointer != types.t.strukt->defaultIsPointer())
			todo<void>("compare by value -- just take a ref and compare by ref");

		if (has(types.t.strukt->special)) {
			const SpecialStructInfo info = force(types.t.strukt->special);
			switch (info.kind) {
				case SpecialStructInfo::Kind::arr:
					return generateCompareArr(
						ctx,
						compareFunInst,
						types.comparison,
						compareFun,
						types,
						info.elementType,
						a,
						b);
				default:
					assert(0);
			}
		}

		return types.t.strukt->body().match(
			[&](const ConcreteStructBody::Builtin builtin) {
				const BuiltinStructInfo info = builtin.info;
				switch (info.kind) {
					case BuiltinStructKind::_bool:
					case BuiltinStructKind::byte:
					case BuiltinStructKind::_char:
					case BuiltinStructKind::float64:
					case BuiltinStructKind::int16:
					case BuiltinStructKind::int32:
					case BuiltinStructKind::int64:
					case BuiltinStructKind::nat16:
					case BuiltinStructKind::nat32:
					case BuiltinStructKind::nat64:
					case BuiltinStructKind::ptr: {
						// Output: a < b ? less : b < a ? greater : equal
						const ConcreteExpr* aLessB = makeLess(arena, boolType, a, b);
						const ConcreteExpr* bLessA = makeLess(arena, boolType, b, a);
						const ConcreteExpr* elze = makeCond(
							arena,
							types.comparison,
							bLessA,
							genGreaterLiteral(arena, types),
							genEqualLiteral(arena, types));
						const ConcreteExpr* expr = makeCond(
							arena,
							types.comparison,
							aLessB,
							genLessLiteral(arena, types),
							elze);
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
				return generateCompareRecord(ctx, compareFunInst, types, r, a, b, aIsPointer, bIsPointer);
			},
			[&](const ConcreteStructBody::Union) {
				return todo<const ConcreteFunExprBody>("compare union");
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
