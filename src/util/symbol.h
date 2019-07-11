#include "./mutDict.h"
#include "./str.h"

struct Symbol {
	// For small strings: the value is a packed representation of the string.
	// For large strings: the value if from 'nextLargeStringId'.
	Nat64 value;
};

struct Symbols {
	Arena arena;
	MutDict<const Str, const Symbol, compareStr> largeStrings;
	Nat64 nextLargeStringId;
};

const Symbol getSymbol(Symbols* symbols, const Str str);

const Str strOfSymbol(Arena& arena, Symbols* symbols, const Symbol symbol);
