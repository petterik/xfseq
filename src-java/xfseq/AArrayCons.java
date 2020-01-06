package xfseq;

import clojure.lang.*;

public abstract class AArrayCons extends ASeq implements IChunkedSeq, IChunk {

    final int _offset;
    final int _end;
    final ISeq _more;

    public AArrayCons(IPersistentMap meta, int offset, int end, ISeq more) {
        super(meta);
        this._offset = offset;
        this._end = end;
        this._more = more;
    }

    @Override
    public IChunk chunkedFirst() {
        return this;
    }

    @Override
    public ISeq chunkedNext() {
        return chunkedMore().seq();
    }

    @Override
    public ISeq chunkedMore() {
        return _more == null ? PersistentList.EMPTY : _more;
    }

    @Override
    public ISeq next() {
        return more().seq();
    }

    @Override
    public int count() {
        return _end - _offset;
    }
}
