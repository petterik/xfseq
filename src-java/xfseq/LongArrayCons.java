package xfseq;

import clojure.lang.*;

public class LongArrayCons extends AArrayCons implements ILongChunk, ILongSeq {

    private final long[] array;

    public LongArrayCons(IPersistentMap meta, long[] array, int offset, int end, ISeq more) {
        super(meta, offset, end, more);
        this.array = array;
    }

    public LongArrayCons(long[] array, int offset, int end, ISeq more) {
        super(null, offset, end, more);
        this.array = array;
    }

    @Override
    public Object first() {
        return array[_offset];
    }

    @Override
    public ISeq more() {
        int next = _offset + 1;
        if (next == _end) {
            return _more == null ? PersistentList.EMPTY : _more;
        } else {
            return new LongArrayCons(array, next, _end, _more);
        }
    }

    @Override
    public Obj withMeta(IPersistentMap meta){
        if(meta != meta())
            return new LongArrayCons(meta, array, _offset, _end, _more);
        return this;
    }
    // IChunk

    @Override
    public IChunk dropFirst() {
        if(_offset == _end)
            throw new IllegalStateException("dropFirst of empty chunk");
        return new LongArrayCons(array, _offset + 1, _end, _more);
    }

    @Override
    public Object reduce(IFn f, Object start) {
        Object ret = f.invoke(start, array[_offset]);
        if (RT.isReduced(ret))
            return ret;
        for (int i = _offset + 1; i < _end; i++) {
            ret = f.invoke(start, array[i]);
            if (RT.isReduced(ret)) {
                return ret;
            }
        }
        return ret;
    }

    @Override
    public long firstLong() {
        return array[_offset];
    }

    @Override
    public long nthLong(int i) {
        return array[_offset + i];
    }

    @Override
    public Object nth(int i) {
        return nthLong(i);
    }

    @Override
    public Object nth(int i, Object notFound) {
        if (i >= 0 && i < count()) {
            return array[_offset + i];
        } else {
            return notFound;
        }
    }
}
