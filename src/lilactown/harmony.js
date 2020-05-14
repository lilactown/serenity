/**
 * @fileoverview added by tsickle
 * @suppress {checkTypes,constantProperty,extraRequire,missingOverride,missingReturn,unusedPrivateMembers,uselessCode} checked by tsc
 */
goog.provide('lilactown.harmony');

/** @typedef {string} */
var txID;
/** @typedef {number} */
var refVersion;
/**
 * @record
 * @template T
 */
function IRef() { }
if (false) {
    /** @type {number} */
    IRef.prototype.version;
    /**
     * @return {T}
     */
    IRef.prototype.unsafeRead = function () { };
    /**
     * @param {T} v
     * @return {T}
     */
    IRef.prototype.unsafeWrite = function (v) { };
}
/**
 * @template T
 * @implements {IRef}
 */
class Ref {
    /**
     * @param {T} init
     */
    constructor(init) {
        this.current = init;
        this.version = 0;
    }
    /**
     * @return {T}
     */
    unsafeRead() {
        return this.current;
    }
    /**
     * @param {T} v
     * @return {T}
     */
    unsafeWrite(v) {
        this.current = v;
        this.version++;
        return v;
    }
}
if (false) {
    /**
     * @type {T}
     * @private
     */
    Ref.prototype.current;
    /** @type {number} */
    Ref.prototype.version;
}
/**
 * @record
 */
function IBranchContext() { }
if (false) {
    /** @type {string} */
    IBranchContext.prototype.id;
    /** @type {boolean} */
    IBranchContext.prototype.isWriteable;
    /** @type {!IBranch} */
    IBranchContext.prototype.parent;
    /**
     * @template T
     * @param {!IRef<T>} ref
     * @return {T}
     */
    IBranchContext.prototype.read = function (ref) { };
    /**
     * @template T
     * @param {!IRef<T>} ref
     * @param {T} v
     * @return {T}
     */
    IBranchContext.prototype.write = function (ref, v) { };
    /**
     * @return {?}
     */
    IBranchContext.prototype.getRefEntries = function () { };
}
/** @type {number} */
let _idSrc = 0;
/**
 * @return {string}
 */
function nextTxId() {
    return "tx" + _idSrc++;
}
/** @typedef {!Map<!IRef<?>, {value: ?, version: number}>} */
var RefMap;
/**
 * @implements {IBranchContext}
 */
class BranchContext {
    /**
     * @param {!IBranch} parent
     */
    constructor(parent) {
        this.id = nextTxId();
        this.currentRefValues = new Map();
        this.isWriteable = true;
        this.parent = parent;
    }
    /**
     * @template T
     * @param {!IRef<T>} ref
     * @return {T}
     */
    read(ref) {
        var _a;
        if (this.currentRefValues.has(ref)) {
            return (_a = this.currentRefValues.get(ref)) === null || _a === void 0 ? void 0 : _a.value;
        }
        // should we add read values to tx??
        if (this.isWriteable) {
            return this.write(ref, ref.unsafeRead());
        }
        return ref.unsafeRead();
    }
    /**
     * @template T
     * @param {!IRef<T>} ref
     * @param {T} v
     * @return {T}
     */
    write(ref, v) {
        if (!this.isWriteable) {
            throw new Error("Cannot change ref inside of doIn");
        }
        // TODO add check here for drift
        this.currentRefValues.set(ref, { value: v, version: ref.version });
        return v;
    }
    /**
     * @return {!IterableIterator<!Array<?>>}
     */
    getRefEntries() {
        return this.currentRefValues.entries();
    }
}
if (false) {
    /** @type {string} */
    BranchContext.prototype.id;
    /** @type {!Map<!IRef<?>, {value: ?, version: number}>} */
    BranchContext.prototype.currentRefValues;
    /** @type {boolean} */
    BranchContext.prototype.isWriteable;
    /** @type {!IBranch} */
    BranchContext.prototype.parent;
}
/**
 * @record
 */
function IBranch() { }
lilactown.harmony.IBranch = IBranch;
if (false) {
    /** @type {boolean} */
    IBranch.prototype.isCommitted;
    /** @type {boolean} */
    IBranch.prototype.isAborted;
    /**
     * @param {function(): ?} thunk
     * @return {!IBranch}
     */
    IBranch.prototype.add = function (thunk) { };
    /**
     * @template T
     * @param {function(): T} f
     * @return {T}
     */
    IBranch.prototype.doIn = function (f) { };
    /**
     * @return {!IBranch}
     */
    IBranch.prototype.rebase = function () { };
    /**
     * @return {!IBranch}
     */
    IBranch.prototype.flush = function () { };
    /**
     * @return {!IBranch}
     */
    IBranch.prototype.flushNext = function () { };
    /**
     * @return {!IBranch}
     */
    IBranch.prototype.commit = function () { };
}
/** @type {{current: (undefined|!IBranchContext)}} */
let ctx = {
    current: undefined,
};
/** @type {*} */
let rebaseSignal = {};
/**
 * @implements {IBranch}
 */
class Branch {
    /**
     * @param {boolean} autoRebase
     * @param {(undefined|!IBranchContext)=} context
     */
    constructor(autoRebase, context) {
        this.context = context || new BranchContext(this);
        this.unrealizedThunks = [];
        this.realizedThunks = [];
        this.isCommitted = false;
        this.isAborted = false;
        this.autoRebase = autoRebase;
    }
    /**
     * @return {boolean}
     */
    isParentBranch() {
        return this.context.parent === this;
    }
    /**
     * @template THIS
     * @this {THIS}
     * @param {function(): ?} thunk
     * @return {THIS}
     */
    add(thunk) {
        if ((/** @type {!Branch} */ (this)).isCommitted) {
            throw new Error("Cannot add to transaction which has been committed");
        }
        if ((/** @type {!Branch} */ (this)).isAborted) {
            throw new Error("Cannot add to transaction which has been aborted. Rebase it first");
        }
        (/** @type {!Branch} */ (this)).unrealizedThunks.push(thunk);
        return (/** @type {!Branch} */ (this));
    }
    /**
     * @template T
     * @param {function(): T} f
     * @return {T}
     */
    doIn(f) {
        /** @type {?} */
        let v;
        this.context.isWriteable = false;
        ctx.current = this.context;
        // TODO find some way to disable writing in doIn
        try {
            v = f();
        }
        catch (e) {
            if (e === rebaseSignal) {
                throw new Error("Executions in doIn cannot cause a rebase.");
            }
            else
                throw e;
        }
        ctx.current = undefined;
        this.context.isWriteable = true;
        return v;
    }
    /**
     * @return {!IBranch}
     */
    flushNext() {
        let [thunk, ...rest] = this.unrealizedThunks;
        this.realizedThunks.push(thunk);
        this.unrealizedThunks = rest;
        /** @type {?} */
        let error;
        ctx.current = this.context;
        try {
            thunk();
        }
        catch (e) {
            error = e;
        }
        ctx.current = undefined;
        if (error === rebaseSignal) {
            if (this.isParentBranch()) {
                this.rebase();
            }
            // not sure if we should throw or just continue...
            throw new Error("Tranasction was restarted; rebasing");
        }
        else if (error) {
            this.isAborted = true;
            throw error;
        }
        return this;
    }
    /**
     * @return {!IBranch}
     */
    flush() {
        while (!this.isAborted && this.unrealizedThunks.length) {
            this.flushNext();
        }
        return this;
    }
    /**
     * @template THIS
     * @this {THIS}
     * @return {THIS}
     */
    rebase() {
        if ((/** @type {!Branch} */ (this)).isCommitted) {
            throw new Error("Cannot rebase transaction which has been committed");
        }
        // general strategy atm is to move all thunks into unrealized state
        // reset context and then exec them at a later time
        (/** @type {!Branch} */ (this)).unrealizedThunks = (/** @type {!Branch} */ (this)).realizedThunks.map((/**
         * @param {!Function} fn
         * @return {!Function}
         */
        (fn) => fn));
        // this should never be reached by a nested tx
        if ((/** @type {!Branch} */ (this)).context.parent !== (/** @type {!Branch} */ (this))) {
            throw new Error("Invariant: Nested transaction should never be rebased");
        }
        (/** @type {!Branch} */ (this)).context = new BranchContext((/** @type {!Branch} */ (this)));
        (/** @type {!Branch} */ (this)).isAborted = false;
        return (/** @type {!Branch} */ (this));
    }
    /**
     * @return {!IBranch}
     */
    commit() {
        if (this.isCommitted) {
            throw new Error("Cannot commit transaction which has already been committed");
        }
        if (this.isAborted) {
            throw new Error("Cannot commit transaction which has been aborted. Rebase it first");
        }
        // realize any left over thunks
        this.flush();
        try {
            for (let refEntry of this.context.getRefEntries()) {
                let [ref, current] = refEntry;
                if (ref.version !== current.version) {
                    // drift occurred, rebase
                    throw rebaseSignal;
                }
                if (this.isParentBranch()) {
                    ref.unsafeWrite(current.value);
                }
            }
        }
        catch (e) {
            if (e === rebaseSignal) {
                if (this.isParentBranch()) {
                    this.rebase();
                    if (this.autoRebase) {
                        return this.commit();
                    }
                    throw new Error("Drift has occurred. Transaction rebased");
                }
                else {
                    // bubble up rebase
                    throw e;
                }
            }
        }
        this.isCommitted = true;
        return this;
    }
}
if (false) {
    /**
     * @type {!IBranchContext}
     * @private
     */
    Branch.prototype.context;
    /**
     * @type {!Array<!Function>}
     * @private
     */
    Branch.prototype.unrealizedThunks;
    /**
     * @type {!Array<!Function>}
     * @private
     */
    Branch.prototype.realizedThunks;
    /** @type {boolean} */
    Branch.prototype.isCommitted;
    /** @type {boolean} */
    Branch.prototype.isAborted;
    /** @type {boolean} */
    Branch.prototype.autoRebase;
}
/**
 * @param {{autoRebase: boolean}=} __0
 * @return {!IBranch}
 */
function branch({ autoRebase } = { autoRebase: false }) {
    return new Branch(autoRebase, ctx.current);
}
lilactown.harmony.branch = branch;
/**
 * @template T
 * @param {T} v
 * @return {!IRef<T>}
 */
function ref(v) {
    return new Ref(v);
}
lilactown.harmony.ref = ref;
/**
 * @template T
 * @param {!IRef<T>} ref
 * @return {T}
 */
function deref(ref) {
    if (ctx.current) {
        return ctx.current.read(ref);
    }
    return ref.unsafeRead();
}
lilactown.harmony.deref = deref;
/**
 * @template T
 * @param {!IRef<T>} ref
 * @param {T} v
 * @return {T}
 */
function set(ref, v) {
    if (ctx.current) {
        return ctx.current.write(ref, v);
    }
    throw new Error("Cannot set ref outside of transaction");
}
lilactown.harmony.set = set;
/**
 * @template T
 * @param {!IRef<T>} ref
 * @param {function(T, ...?): T} f
 * @param {...?} args
 * @return {T}
 */
function alter(ref, f, ...args) {
    return set(ref, f(deref(ref), ...args));
}
lilactown.harmony.alter = alter;
/**
 * @template T
 * @param {!IRef<T>} ref
 * @return {T}
 */
function ensure(ref) {
    return set(ref, deref(ref));
}
lilactown.harmony.ensure = ensure;
