package xfseq;

import clojure.lang.*;

public class DoubleArrayCons extends AArrayCons implements IDoubleChunk, IDoubleSeq {

    private final double[] array;

    public DoubleArrayCons(IPersistentMap meta, double[] array, int offset, int end, ISeq more) {
        super(meta, offset, end, more);
        this.array = array;
    }

    public DoubleArrayCons(double[] array, int offset, int end, ISeq more) {
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
            return new DoubleArrayCons(array, next, _end, _more);
        }
    }

    @Override
    public Obj withMeta(IPersistentMap meta){
        if(meta != meta())
            return new DoubleArrayCons(meta, array, _offset, _end, _more);
        return this;
    }
    // IChunk

    @Override
    public IChunk dropFirst() {
        if(_offset == _end)
            throw new IllegalStateException("dropFirst of empty chunk");
        return new DoubleArrayCons(array, _offset + 1, _end, _more);
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
    public double firstDouble() {
        return array[_offset];
    }

    @Override
    public double nthDouble(int i) {
        return array[_offset + i];
    }

    @Override
    public Object nth(int i) {
        return nthDouble(i);
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
