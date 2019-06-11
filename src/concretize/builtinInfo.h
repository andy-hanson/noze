#pragma once

#include "../concreteModel.h"
#include "../model.h"

const BuiltinFunInfo getBuiltinFunInfo(const Sig sig);
const BuiltinStructInfo getBuiltinStructInfo(const StructDecl* s);
