#pragma once

#include "../concreteModel.h"

// We set things as referenced *after* we've concretized the whole program.
// This is because some things we generate may end up not being used --
// for example, we create a constant, then we end up eliminating that later.
// Since constants can contain lambdas / types, that requires us to do them all in a second step.
const ConcreteProgram getReferencedOnly(
	Arena* arena,
	const ConcreteFun* rtMain,
	const ConcreteFun* userMain,
	const ConcreteStruct* ctxStruct);
