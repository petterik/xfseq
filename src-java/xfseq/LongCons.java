package xfseq;

import clojure.lang.*;

public class LongCons extends ASeq implements ILongSeq {

    private final long _first;
    private final ISeq _more;

    public LongCons(long first, ISeq _more) {
        this._first = first;
        this._more = _more;
    }

    public LongCons(IPersistentMap meta, long _first, ISeq _more) {
        super(meta);
        this._first = _first;
        this._more = _more;
    }

    @Override
    public Object first() {
        return _first;
    }

    @Override
    public ISeq next() {
        return _more;
    }

    @Override
    public LongCons withMeta(IPersistentMap meta) {
        return this.meta() == meta ? this : new LongCons(meta, this._first, this._more);
    }

    @Override
    public long firstLong() {
        return _first;
    }
}
