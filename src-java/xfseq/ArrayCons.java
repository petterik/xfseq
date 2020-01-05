package xfseq;

import clojure.lang.*;

public class ArrayCons extends ASeq implements IChunkedSeq, IChunk {

    private final Object[] array;
    private final int offset;
    private final int end;
    private final ISeq more;

    public ArrayCons(IPersistentMap meta, Object[] array, int offset, int end, ISeq more) {
        super(meta);
        this.array = array;
        this.offset = offset;
        this.end = end;
        this.more = more;
    }

    public ArrayCons(Object[] array, int offset, int end, ISeq more) {
        super(null);
        this.array = array;
        this.offset = offset;
        this.end = end;
        this.more = more;
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
        return more == null ? PersistentList.EMPTY : more;
    }

    @Override
    public Object first() {
        return array[offset];
    }

    @Override
    public ISeq next() {
        return more().seq();
    }

    @Override
    public ISeq more() {
        int next = offset + 1;
        if (next == end) {
            return more == null ? PersistentList.EMPTY : more;
        } else {
            return new ArrayCons(array, next, end, more);
        }
    }

    @Override
    public Obj withMeta(IPersistentMap meta){
        if(meta != meta())
            return new ArrayCons(meta, array, offset, end, more);
        return this;
    }

    // IChunk

    @Override
    public IChunk dropFirst() {
        if(offset == end)
            throw new IllegalStateException("dropFirst of empty chunk");
        return new ArrayCons(array, offset + 1, end, more);
    }

    @Override
    public Object reduce(IFn f, Object start) {
        Object ret = f.invoke(start, array[offset]);
        if (RT.isReduced(ret))
            return ret;
        for (int i = offset + 1; i < end; i++) {
            ret = f.invoke(start, array[i]);
            if (RT.isReduced(ret)) {
                return ret;
            }
        }
        return ret;
    }

    @Override
    public Object nth(int i) {
        return array[offset + i];
    }

    @Override
    public Object nth(int i, Object notFound) {
        if (i >= 0 && i < count()) {
            return array[offset + i];
        } else {
            return notFound;
        }
    }

    @Override
    public int count() {
        return end - offset;
    }
}
