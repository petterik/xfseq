package xfseq;

import clojure.lang.*;

public class LongCons extends ACons implements ILongSeq {

    private final long _first;

    public LongCons(long first, ISeq more) {
        super(more);
        this._first = first;

    }

    public LongCons(IPersistentMap meta, long first, ISeq more) {
        super(meta, more);
        this._first = first;
    }

    @Override
    public Object first() {
        return _first;
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
