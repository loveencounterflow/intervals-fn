import {
  any,
  aperture,
  applySpec,
  concat,
  converge,
  drop,
  dropWhile,
  either,
  groupWith,
  head,
  isEmpty,
  isNil,
  map,
  nthArg,
  pipe,
  prop,
  reject,
  sortBy,
  unfold,
  unnest,
} from 'ramda';

// =========================================================================================================
export type roat<T> = ReadonlyArray<T>;

export interface IntervalSE {
  lo: number;
  hi: number;
}

export type IntervalAR = [number, number];


/**
 * Complement of `intervals` bounded to `boundaries`. Convert space between two consecutive intervals into interval.
 * Keeps extra object properties on `boundaries`.
 * intervals array has to be sorted.
 * Doesn't mutate input. Output keeps input's structure.
 *
 * boundaries | interval(s) | result
 * --- | --- | ---
 * { lo: 0, hi: 10} | [{ lo: 3, hi: 7 }] | [{ lo: 0, hi: 3 }, { lo: 7, hi: 10 }]
 * { lo: 0, hi: 10} | [{ lo: 2, hi: 4 }, { lo: 7, hi: 8 }] | [{ lo: 0, hi: 2 }, { lo: 4, hi: 7 }, { lo: 8, hi: 10 }]
 *
 * @param boundaries arg1: interval defining boundaries for the complement computation.
 * @param intervals arg2: array of intervals that complement the result.
 * @returns array of intervals.
 */
export const complement = <T extends IntervalSE>(
  boundaries: T,
  intervals: roat<IntervalSE>
): T[] => {
  const { lo, hi, ...rest }: IntervalSE = boundaries as any; // See TypeScript/pull/13288 TypeScript/issues/10727
  const prepRanges: IntervalSE[] = [
    { lo: -Infinity, hi: lo },
    ...intervals,
    { lo: hi, hi: Infinity },
  ];
  return reject<IntervalSE | null>(
    isNil,
    // @ts-ignore
    aperture(2, prepRanges).map(
      ([r1, r2]) => (r1.hi >= r2.lo ? null : { lo: r1.hi, hi: r2.lo, ...rest })
    )
  ) as unknown as T[];
};

/**
 * Test if `intervalA` overlaps with `intervalB`.
 *
 * intervalA | intervalB | result
 * --- | --- | ---
 * { lo: 0, hi: 10} | { lo: 3, hi: 7 } | true
 * { lo: 0, hi: 5} | { lo: 5, hi: 7 } | false
 *
 * @param intervalA arg1: interval
 * @param intervalB arg2: interval
 * @returns true if overlaps
 */
export const isOverlappingSimple = (a: IntervalSE, b: IntervalSE): boolean => {
  return b.lo < a.hi && b.hi > a.lo;
};

const isOverlappingNum = (a: IntervalSE, b: number): boolean => {
  return a.lo < b && b < a.hi;
};

const beforeOrAdjTo = (afterInt: IntervalSE) => (beforeInt: IntervalSE) =>
  beforeInt.hi <= afterInt.lo;

/**
 * Test if `intervalA` overlaps with `intervalB`.
 *
 * Accept array of intervals.
 * Intervals arrays have to be sorted.
 *
 * intervalA | intervalB | result
 * --- | --- | ---
 * { lo: 0, hi: 10} | { lo: 3, hi: 7 } | true
 * { lo: 0, hi: 5} | { lo: 5, hi: 7 } | false
 * { lo: 5, hi: 10} | [{ lo: 0, hi: 4 }, { lo: 7, hi: 8 }] | true
 *
 * @param intervalA arg1: interval or array of intervals
 * @param intervalB arg2: interval or array of intervals
 * @returns true if overlaps
 */
export const isOverlapping = (
  intervalsA: roat<IntervalSE>,
  intervalsB: roat<IntervalSE>
): boolean => {
  if ([intervalsA, intervalsB].some(isEmpty)) {
    return false;
  }
  const intsA = intervalsA[0];
  const newInters2 = dropWhile(beforeOrAdjTo(intsA), intervalsB);
  if (isEmpty(newInters2)) {
    return false;
  }
  const intsB = newInters2[0];
  return isOverlappingSimple(intsA, intsB) ? true : isOverlapping(drop(1, intervalsA), newInters2);
};

/**
 * Test if `intervalA` is adjacent to (meets) `intervalB`.
 *
 * intervalA | intervalB | result
 * --- | --- | ---
 * { lo: 0, hi: 10} | { lo: 3, hi: 7 } | false
 * { lo: 0, hi: 5} | { lo: 5, hi: 7 } | true
 *
 * @param intervalA arg1: interval
 * @param intervalB arg2: interval
 * @returns true if adjacent
 */
export const isMeeting = (a: IntervalSE, b: IntervalSE): boolean => {
  return a.lo === b.hi || a.hi === b.lo;
};

/**
 * Test if `intervalA` is before or adjacent `intervalB`.
 *
 * intervalA | intervalB | result
 * --- | --- | ---
 * { lo: 0, hi: 2} | { lo: 3, hi: 7 } | true
 * { lo: 0, hi: 5} | { lo: 3, hi: 7 } | false
 *
 * @param intervalA arg1: interval
 * @param intervalB arg2: interval
 * @returns true if before
 */
export const isBefore = (a: IntervalSE, b: IntervalSE): boolean => {
  return a.hi <= b.lo;
};

/**
 * Test if `intervalA` is after or adjacent `intervalB`.
 *
 * intervalA | intervalB | result
 * --- | --- | ---
 * { lo: 5, hi: 10} | { lo: 3, hi: 4 } | true
 * { lo: 5, hi: 10} | { lo: 3, hi: 6 } | false
 *
 * @param intervalA arg1: interval
 * @param intervalB arg2: interval
 * @returns true if after
 */
export const isAfter = (a: IntervalSE, b: IntervalSE): boolean => {
  return a.lo >= b.hi;
};

/**
 * Test if `intervalA` and `intervalB` share the same starting point.
 *
 * intervalA | intervalB | result
 * --- | --- | ---
 * { lo: 5, hi: 10} | { lo: 5, hi: 4 } | true
 * { lo: 5, hi: 10} | { lo: 0, hi: 10 } | false
 *
 * @param intervalA arg1: interval
 * @param intervalB arg2: interval
 * @returns true if same starting point
 */
export const isStarting = (a: IntervalSE, b: IntervalSE): boolean => {
  return a.lo === b.lo;
};

/**
 * Test if `intervalA` and `intervalB` share the same ending point.
 *
 * intervalA | intervalB | result
 * --- | --- | ---
 * { lo: 5, hi: 10} | { lo: 0, hi: 10 } | true
 * { lo: 5, hi: 10} | { lo: 5, hi: 7 } | false
 *
 * @param intervalA arg1: interval
 * @param intervalB arg2: interval
 * @returns true if same ending point
 */
export const isEnding = (a: IntervalSE, b: IntervalSE): boolean => {
  return a.hi === b.hi;
};

/**
 * Test if `intervalA` occurs in `intervalB`. `intervalsB` act as boundaries. Can share starting and/or ending point.
 *
 * intervalA | intervalB | result
 * --- | --- | ---
 * { lo: 2, hi: 6} | { lo: 0, hi: 10 } | true
 * { lo: 5, hi: 10} | { lo: 0, hi: 10 } | true
 * { lo: 5, hi: 10} | { lo: 0, hi: 9 } | false
 *
 * @param intervalA arg1: interval
 * @param intervalB arg2: interval
 * @returns true if `intervalA` occurs in `intervalB`
 */
export const isDuring = (a: IntervalSE, b: IntervalSE): boolean => {
  return a.lo >= b.lo && a.hi <= b.hi;
};

/**
 * Test if `intervalA` is equivalent to `intervalB`.
 *
 * intervalA | intervalB | result
 * --- | --- | ---
 * { lo: 5, hi: 10} | { lo: 5, hi: 10 } | true
 * { lo: 5, hi: 10} | { lo: 0, hi: 10 } | false
 *
 * @param intervalA arg1: interval
 * @param intervalB arg2: interval
 * @returns true if equivalent
 */
export const isEqual = (a: IntervalSE, b: IntervalSE): boolean => {
  return a.lo === b.lo && a.hi === b.hi;
};

const propFromNthArg = (n: number, propName: string) =>
  pipe(
    nthArg(n),
    prop(propName)
  );
const maxEnd = (ranges: IntervalSE[]) => ranges.reduce((a, b) => (a.hi > b.hi ? a : b));

const simplifyPipe = pipe(
  groupWith(either(isOverlappingSimple, isMeeting)),
  map(
    converge(
      applySpec<IntervalSE>({ lo: propFromNthArg(0, 'lo'), hi: propFromNthArg(1, 'hi') }),
      [head, maxEnd]
    )
  )
) as (a: IntervalSE[]) => IntervalSE[];

/**
 * Simplification of `intervals`. Unify touching or overlapping intervals.
 *
 * Intervals array has to be sorted.
 *
 * Doesn't mutate input. Output keeps input's structure.
 *
 * | intervals A | result |
 * | ----------- | ------ |
 * | [{ lo: 3, hi: 9 }, { lo: 9, hi: 13 }, { lo: 11, hi: 14 }] | [{ lo: 3, hi: 14 }] |
 *
 * @param intervalA
 */
export const simplify = <T extends IntervalSE>(intervals: roat<T>) =>
  simplifyPipe([...intervals]) as T[];

const sortByStart = sortBy<IntervalSE>(prop('lo'));

const unifyPipe = pipe(
  concat as (a: IntervalSE[], b: IntervalSE[]) => IntervalSE[],
  sortByStart,
  simplify
) as (a: IntervalSE[], b: IntervalSE[]) => IntervalSE[];

/**
 * Union of `intervals`.
 *
 * Accept array of intervals. Doesn't mutate input. Output keeps input's structure.
 * Intervals arrays have to be sorted.
 *
 * interval(s) A | interval(s) B | result
 * --- | --- | ---
 * [{ lo: 0, hi: 4}] | [{ lo: 3, hi: 7 }, { lo: 9, hi: 11 }] | [{ lo: 0, hi: 7 }, { lo: 9, hi: 11 }]
 *
 * @param intervalA arg1: array of intervals
 * @param intervalB arg2: array of intervals
 * @returns union of `arg1` and `arg2`
 */
export const unify = <T extends IntervalSE>(intervalsA: roat<T>, intervalsB: roat<T>) =>
  unifyPipe([...intervalsA], [...intervalsB]);

const intersectUnfolderSeed = (
  i1: IntervalSE[],
  i2: IntervalSE[]
): [IntervalSE[], IntervalSE[]] => {
  const new1 = i1[0].hi > i2[0].hi ? i1 : drop(1, i1);
  const new2 = i2[0].hi > i1[0].hi ? i2 : drop(1, i2);
  return [new1, new2];
};

const intersectUnfolder = ([inters1, inters2]: [roat<IntervalSE>, roat<IntervalSE>]):
  | false
  | [IntervalSE | null, [IntervalSE[], IntervalSE[]]] => {
  if (any(isEmpty)([inters1, inters2])) {
    return false;
  }
  const newInters1 = dropWhile(beforeOrAdjTo(inters2[0]), inters1);
  if (isEmpty(newInters1)) {
    return false;
  }
  const inter1 = newInters1[0];
  const newInters2 = dropWhile(beforeOrAdjTo(inter1), inters2);
  if (isEmpty(newInters2)) {
    return false;
  }
  const inter2 = newInters2[0];
  const minMaxInter = {
    ...inter2,
    hi: Math.min(inter1.hi, inter2.hi),
    lo: Math.max(inter1.lo, inter2.lo),
  };
  const resultInter = beforeOrAdjTo(minMaxInter)(minMaxInter) ? null : minMaxInter;
  const seed = intersectUnfolderSeed(newInters1, newInters2);
  return [resultInter, seed];
};

/**
 * Intersection of `intervals`. Does not simplify result. Keeps extra object properties on `intervalB`.
 *
 * `interalA` and `interalB` can have different structure.
 * Accept array of intervals. Doesn't mutate input. Output keeps `intervalB` structure.
 * Intervals arrays have to be sorted.
 *
 * interval(s) A | interval(s) B | result
 * --- | --- | ---
 * { lo: 0, hi: 4 } | { lo: 3, hi: 7, foo: 'bar' } | [{ lo: 3, hi: 4, foo: 'bar' }]
 * { lo: 0, hi: 10 } | [{ lo: 2, hi: 5}, { lo: 5, hi: 8}] | [{ lo: 2, hi: 5 }, { lo: 5, hi: 8 }]
 * [{ lo: 0, hi: 4 }, { lo: 8, hi: 11 }] | [{ lo: 2, hi: 9 }, { lo: 10, hi: 13 }] | [{ lo: 2, hi: 4 }, { lo: 8, hi: 9 }, { lo: 10, hi: 11 }]
 *
 * @param intervalA arg1: array of intervals
 * @param intervalB arg2: array of intervals
 * @returns intersection of `arg1` and `arg2`
 */
export const intersect = <T extends IntervalSE>(
  intervalsA: roat<IntervalSE>,
  intervalsB: roat<T>
): T[] => {
  return unfold(intersectUnfolder, [intervalsA, intervalsB] as [
    roat<IntervalSE>,
    roat<IntervalSE>
  ]).filter(i => i != null) as T[];
};

const minStart = (ranges: roat<IntervalSE>) => ranges.reduce((a, b) => (a.lo < b.lo ? a : b));

const mergeUnfolder = (mergeFn: (ints: any[]) => any) => (
  ints: roat<IntervalSE>
): false | [any, roat<IntervalSE>] => {
  if (!ints.length) {
    return false;
  }
  const lo = minStart(ints).lo;
  const withoutStart = ints
    .filter(a => a.hi > lo)
    .map(a => (a.lo === lo ? { ...a, lo: a.hi } : a));
  const hi = minStart(withoutStart).lo;
  const toMerge = ints.filter(a => isDuring({ lo, hi }, a));
  const next = { ...mergeFn(toMerge), lo, hi };
  return [
    next,
    ints.filter(a => a.hi > hi).map(a => (a.lo <= hi ? { ...a, lo: hi } : a)),
  ];
};

/**
 * Merge extra properties of all intervals inside `intervals`, when overlapping, with provided function `mergeFn`.
 * Can also be used to generate an array of intervals without overlaps
 *
 * Doesn't mutate input. Output keeps input's structure.
 * Interval array has to be sorted.
 *
 * parameter | value
 * --- | ---
 * mergeFn | `(a, b) => {...a, data: a.data + b.data }`
 * intervals | `[{ lo: 0, hi: 10, data: 5 }, { lo: 4, hi: 7, data: 100 }]`
 * result | `[{ lo: 0, hi: 4, data: 5 }, { lo: 4, hi: 7, data: 105 }, { lo: 7, hi: 10, data: 5 }]`
 * @param mergeFn arg1: function to merge extra properties of overlapping intervals
 * @param intervals arg2: intervals with extra properties.
 */
export const merge = <T extends IntervalSE>(
  mergeFn: (ints: any[]) => any,
  intervals: roat<T>
): T[] => {
  return unfold(mergeUnfolder(mergeFn), intervals) as T[];
};

const subtractInter = (mask: IntervalSE[], base: IntervalSE): IntervalSE[] => {
  return complement(base, mask);
};

/**
 * Subtact `base` with `mask`.
 * Keeps extra object properties on `base`.
 *
 * Accept array of intervals. Doesn't mutate input. Output keeps input's structure.
 * Intervals arrays have to be sorted.
 *
 * interval(s) base | interval(s) mask | result
 * --- | --- | ---
 * [{ lo: 0, hi: 4 }] | [{ lo: 3, hi: 7 }] | [{ lo: 0, hi: 3 }]
 * [{ lo: 0, hi: 4 }, { lo: 8, hi: 11 }] | [{ lo: 2, hi: 9 }, { lo: 10, hi: 13 }] | [{ lo: 0, hi: 2 }, { lo: 9, hi: 10 }]
 *
 * @param intervalA arg1: array of intervals
 * @param intervalB arg2: array of intervals
 * @returns intersection of `arg1` and `arg2`
 */
export const substract = <T extends IntervalSE>(base: roat<T>, mask: roat<IntervalSE>): T[] => {
  const intersection = intersect(mask, base);
  return unnest(
    base.map(b => subtractInter(intersection.filter(isOverlappingSimple.bind(null, b)), b))
  ) as T[];
};

const splitIntervalWithIndex = (int: IntervalSE, index: number): IntervalSE[] => {
  if (!isOverlappingNum(int, index)) {
    return [int];
  }
  return [{ ...int, lo: int.lo, hi: index }, { ...int, lo: index, hi: int.hi }];
};

/**
 * Split `intervals` with `splitIndexes`.
 * Keeps extra object properties on `intervals`.
 * Doesn't mutate input. Output keeps input's structure.
 *
 * splitIndexes | interval(s) | result
 * --- | --- | ---
 * [2, 4] | { lo: 0, hi: 6, foo: 'bar' } | [{ lo: 0, hi: 2, foo: 'bar' }, { lo: 2, hi: 4, foo: 'bar' } { lo: 4, hi: 6, foo: 'bar' }]
 * [5] | [{ lo: 0, hi: 7 }, { lo: 3, hi: 8 }] | [{ lo: 0, hi: 5 }, { lo: 5, hi: 7 }, { lo: 3, hi: 5 }, { lo: 5, hi: 8 }]
 *
 * @param splitIndexes arg1: defines indexes where intervals are splitted.
 * @param intervals arg2: intervals to be splitted.
 * @returns array of intervals.
 */
export const split = <T extends IntervalSE>(splits: roat<number>, intervals: roat<T>): T[] => {
  if (splits.length < 1 || intervals.length < 1) {
    return intervals as T[];
  }
  return unnest(
    intervals.map(int =>
      splits.reduce(
        (acc: IntervalSE[], i: number) => {
          const lastInt = acc.pop() as T;
          return [...acc, ...splitIntervalWithIndex(lastInt, i)];
        },
        [int]
      )
    )
  ) as T[];
};
