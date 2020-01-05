package xfseq;

// TODO: Something went wrong when refactoring from InlineStep.

import clojure.lang.*;

public class XFSeqStep extends AFn {

    // Note: Changing this from 2, 4 and 8, sees huge difference
    //       in performance (java8).
    //       Setting it to 8 makes it faster on map inc and filter even
    //       compared to vanilla lazy-seqs
    private static final int MIN_SIZE = 8;
    private static final Object[] NULLS = new Object[32];

    private Object[] arr;
    private int idx;
    private int capacity;

    private final IFn xf;
    private ISeq s;

    public XFSeqStep(IFn xf, ISeq s) {
        this.s = s;
        this.idx = 0;
        this.capacity = MIN_SIZE;
        this.arr = new Object[capacity];

        this.xf = (IFn)xf.invoke(this);
    }

    public Object invoke() {
        for(ISeq c = this.s.seq(); c != null; c = c.seq()) {
            if (c instanceof IChunkedSeq) {
                IChunkedSeq cs = (IChunkedSeq) c;
                IChunk ch = cs.chunkedFirst();

                for (int i = 0; i < ch.count(); i++) {
                    if (this != xf.invoke(this, ch.nth(i))) {
                        break;
                    }
                }

                c = cs.chunkedMore();
            } else {
                if (this != xf.invoke(this, c.first())) {
                    break;
                }

                c = c.more();
            }

            if (idx > 0) {
                this.s = c;
                return toSeq(new LazySeq(this));
            }
        }

        xf.invoke(this);
        return idx == 0 ? null : new ArrayCons(arr, 0, idx, null);
    }

    @Override
    public Object invoke(Object a) {
        return a;
    }

    @Override
    public Object invoke(Object a, Object b) {
        if (idx == capacity) {
            capacity *= capacity < 32 ? 4 : 2;
            Object[] larger = new Object[capacity];
            System.arraycopy(arr, 0, larger, 0, idx);
            arr = larger;
        }
        arr[idx++] = b;
        return a;
    }

    private ISeq toSeq(ISeq seq) {
        switch(idx) {
            case 0:
                break;
            // TODO: Verify whether handrolling some cases is a good idea.
            case 1:
                seq = new Cons(arr[0], seq);
                arr[0] = null;
                idx = 0;
                break;
            case 2:
                s = new Cons(arr[0], new Cons(arr[1], seq));
                System.arraycopy(NULLS,0, arr, 0, idx);
                idx = 0;
                break;
            case 3:
                s = new Cons(arr[0], new Cons(arr[1], new Cons(arr[2], seq)));
                System.arraycopy(NULLS,0, arr, 0, idx);
                idx = 0;
                break;
            case 4:
                s = new Cons(arr[0], new Cons(arr[1], new Cons(arr[2], new Cons(arr[3], seq))));
                System.arraycopy(NULLS,0, arr, 0, idx);
                idx = 0;
                break;
            case 5:
            case 6:
            case 7:
            case 8:
            case 9:
            case 10:
            case 11:
            case 12:
            case 13:
            case 14:
            case 15:
            case 16:
            case 17:
            case 18:
            case 19:
            case 20:
            case 21:
            case 22:
            case 23:
            case 24:
            case 25:
            case 26:
            case 27:
            case 28:
            case 29:
            case 30:
            case 31:
            case 32:
                seq = new ArrayCons(arr, 0, idx, seq);
                arr = new Object[capacity];
                idx = 0;
                break;
            default:
                seq = chunkLargeResult(seq);
                capacity = 32;
                arr = new Object[capacity];
                idx = 0;
                break;
        }
        return seq;
    }

    private ISeq chunkLargeResult(ISeq s) {
        // Returns 32 sized chunks in case the transduction created
        // more items than that. When chained, it can blow up.
        // This problem was found with code that repeated interpose:
        // (interpose nil (interpose nil ... (interpose nil (range)) ... ))
        int offset = idx;
        do {
            int end = offset;
            offset = Math.max(0, offset - 32);
            s = new ArrayCons(arr, offset, end, s);
        } while (offset > 0);

        return s;
    }
}
