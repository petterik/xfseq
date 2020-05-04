package xfseq;

import clojure.lang.*;

public abstract class ACons extends ASeq {

    protected ISeq _more;

    public ACons(ISeq more) {
        this._more = more;
    }

    public ACons(IPersistentMap meta, ISeq more) {
        super(meta);
        this._more = more;
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

}
