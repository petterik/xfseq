package xfseq;

import clojure.lang.*;

public class DoubleCons extends ACons implements IDoubleSeq {

    private final double _first;

    public DoubleCons(double first, ISeq _more) {
        super(_more);
        this._first = first;
    }

    public DoubleCons(IPersistentMap meta, double _first, ISeq _more) {
        super(meta, _more);
        this._first = _first;
    }

    @Override
    public Object first() {
        return _first;
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
