package xfseq;

import clojure.lang.*;

public class DoubleCons extends ASeq implements IDoubleSeq {

    private final double _first;
    private final ISeq _more;

    public DoubleCons(double first, ISeq _more) {
        this._first = first;
        this._more = _more;
    }

    public DoubleCons(IPersistentMap meta, double _first, ISeq _more) {
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
        return this.more().seq();
    }

    @Override
    public ISeq more() {
        return this._more == null ? PersistentList.EMPTY : this._more;
    }

    @Override
    public int count() {
        return 1 + RT.count(this._more);
    }

    @Override
    public DoubleCons withMeta(IPersistentMap meta) {
        return this.meta() == meta ? this : new DoubleCons(meta, this._first, this._more);
    }

    @Override
    public double firstDouble() {
        return _first;
    }
}
