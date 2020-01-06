package xfseq;

// TODO: Something went wrong when refactoring from InlineStep.

import clojure.lang.*;

public abstract class AXFSeqStep extends AFn {

    // Note: Changing this from 2, 4 and 8, sees huge difference
    //       in performance (java8).
    //       Setting it to 8 makes it faster on map inc and filter even
    //       compared to vanilla lazy-seqs
    private static final int MIN_SIZE = 8;

    int _idx;
    int _capacity;
    ISeq _s;

    private final IFn xf;

    public AXFSeqStep(IFn xf, ISeq s) {
        this._s = s;
        this._idx = 0;
        this._capacity = MIN_SIZE;

        this.xf = (IFn)xf.invoke(this);
    }

    public Object invoke() {
        for(ISeq c = this._s.seq(); c != null; c = c.seq()) {
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

            if (_idx > 0) {
                this._s = c;
                return toSeq(new LazySeq(this));
            }
        }

        xf.invoke(this);
        return _idx == 0 ? null : toTail();
    }

    @Override
    public Object invoke(Object a) {
        return a;
    }

    protected abstract ISeq toSeq(ISeq more);

    protected abstract ISeq toTail();

    public static class LongSeq extends AXFSeqStep implements IFn.OLO {

        private long[] arr;

        public LongSeq(IFn xf, ISeq s) {
            super(xf, s);
            arr = new long[_capacity];
        }

        @Override
        protected ISeq toTail() {
            return new LongArrayCons(arr, 0, _idx, null);
        }

        public Object invoke(Object a, Object b) {
            return invokePrim(a, (Long)b);
        }

        @Override
        public Object invokePrim(Object a, long b) {
            if (_idx == _capacity) {
                _capacity *= _capacity < 32 ? 4 : 2;
                long[] larger = new long[_capacity];
                System.arraycopy(arr, 0, larger, 0, _idx);
                arr = larger;
            }
            arr[_idx++] = b;
            return a;
        }

        protected ISeq toSeq(ISeq seq) {
            switch (_idx) {
                case 0:
                    break;
                // TODO: Verify whether handrolling some cases is a good idea.
                case 1:
                    seq = new LongCons(arr[0], seq);
                    _idx = 0;
                    break;
                case 2:
                    _s = new LongCons(arr[0], new LongCons(arr[1], seq));
                    _idx = 0;
                    break;
                case 3:
                    _s = new LongCons(arr[0], new LongCons(arr[1], new LongCons(arr[2], seq)));
                    _idx = 0;
                    break;
                case 4:
                    _s = new LongCons(arr[0], new LongCons(arr[1], new LongCons(arr[2], new LongCons(arr[3], seq))));
                    _idx = 0;
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
                    seq = new LongArrayCons(arr, 0, _idx, seq);
                    arr = new long[_capacity];
                    _idx = 0;
                    break;
                default:
                    seq = chunkLargeResult(seq);
                    _capacity = 32;
                    arr = new long[_capacity];
                    _idx = 0;
                    break;
            }
            return seq;
        }

        private ISeq chunkLargeResult(ISeq s) {
            // Returns 32 sized chunks in case the transduction created
            // _more items than that. When chained, it can blow up.
            // This problem was found with code that repeated interpose:
            // (interpose nil (interpose nil ... (interpose nil (range)) ... ))
            int offset = _idx;
            do {
                int end = offset;
                offset = Math.max(0, offset - 32);
                s = new LongArrayCons(arr, offset, end, s);
            } while (offset > 0);

            return s;
        }
    }

    public static class DoubleSeq extends AXFSeqStep implements IFn.ODO {

        private double[] arr;

        public DoubleSeq(IFn xf, ISeq s) {
            super(xf, s);
            arr = new double[_capacity];
        }

        @Override
        protected ISeq toTail() {
            return new DoubleArrayCons(arr, 0, _idx, null);
        }

        public Object invoke(Object a, Object b) {
            return invokePrim(a, (Long)b);
        }

        @Override
        public Object invokePrim(Object a, double b) {
            if (_idx == _capacity) {
                _capacity *= _capacity < 32 ? 4 : 2;
                double[] larger = new double[_capacity];
                System.arraycopy(arr, 0, larger, 0, _idx);
                arr = larger;
            }
            arr[_idx++] = b;
            return a;
        }

        protected ISeq toSeq(ISeq seq) {
            switch (_idx) {
                case 0:
                    break;
                // TODO: Verify whether handrolling some cases is a good idea.
                case 1:
                    seq = new DoubleCons(arr[0], seq);
                    _idx = 0;
                    break;
                case 2:
                    _s = new DoubleCons(arr[0], new DoubleCons(arr[1], seq));
                    _idx = 0;
                    break;
                case 3:
                    _s = new DoubleCons(arr[0], new DoubleCons(arr[1], new DoubleCons(arr[2], seq)));
                    _idx = 0;
                    break;
                case 4:
                    _s = new DoubleCons(arr[0], new DoubleCons(arr[1], new DoubleCons(arr[2], new DoubleCons(arr[3], seq))));
                    _idx = 0;
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
                    seq = new DoubleArrayCons(arr, 0, _idx, seq);
                    arr = new double[_capacity];
                    _idx = 0;
                    break;
                default:
                    seq = chunkLargeResult(seq);
                    _capacity = 32;
                    arr = new double[_capacity];
                    _idx = 0;
                    break;
            }
            return seq;
        }

        private ISeq chunkLargeResult(ISeq s) {
            // Returns 32 sized chunks in case the transduction created
            // _more items than that. When chained, it can blow up.
            // This problem was found with code that repeated interpose:
            // (interpose nil (interpose nil ... (interpose nil (range)) ... ))
            int offset = _idx;
            do {
                int end = offset;
                offset = Math.max(0, offset - 32);
                s = new DoubleArrayCons(arr, offset, end, s);
            } while (offset > 0);

            return s;
        }
    }

    public static class ObjectSeq extends AXFSeqStep {

        private static final Object[] NULLS = new Object[32];

        private Object[] arr;

        public ObjectSeq(IFn xf, ISeq s) {
            super(xf, s);
            arr = new Object[_capacity];
        }

        @Override
        public Object invoke(Object a, Object b) {
            if (_idx == _capacity) {
                _capacity *= _capacity < 32 ? 4 : 2;
                Object[] larger = new Object[_capacity];
                System.arraycopy(arr, 0, larger, 0, _idx);
                arr = larger;
            }
            arr[_idx++] = b;
            return a;
        }

        @Override
        protected ISeq toTail() {
            return new ObjectArrayCons(arr, 0, _idx, null);
        }

        protected ISeq toSeq(ISeq seq) {
            switch (_idx) {
                case 0:
                    break;
                // TODO: Verify whether handrolling some cases is a good idea.
                case 1:
                    seq = new Cons(arr[0], seq);
                    arr[0] = null;
                    _idx = 0;
                    break;
                case 2:
                    _s = new Cons(arr[0], new Cons(arr[1], seq));
                    System.arraycopy(NULLS, 0, arr, 0, _idx);
                    _idx = 0;
                    break;
                case 3:
                    _s = new Cons(arr[0], new Cons(arr[1], new Cons(arr[2], seq)));
                    System.arraycopy(NULLS, 0, arr, 0, _idx);
                    _idx = 0;
                    break;
                case 4:
                    _s = new Cons(arr[0], new Cons(arr[1], new Cons(arr[2], new Cons(arr[3], seq))));
                    System.arraycopy(NULLS, 0, arr, 0, _idx);
                    _idx = 0;
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
                    seq = new ObjectArrayCons(arr, 0, _idx, seq);
                    arr = new Object[_capacity];
                    _idx = 0;
                    break;
                default:
                    seq = chunkLargeResult(seq);
                    _capacity = 32;
                    arr = new Object[_capacity];
                    _idx = 0;
                    break;
            }
            return seq;
        }

        private ISeq chunkLargeResult(ISeq s) {
            // Returns 32 sized chunks in case the transduction created
            // _more items than that. When chained, it can blow up.
            // This problem was found with code that repeated interpose:
            // (interpose nil (interpose nil ... (interpose nil (range)) ... ))
            int offset = _idx;
            do {
                int end = offset;
                offset = Math.max(0, offset - 32);
                s = new ObjectArrayCons(arr, offset, end, s);
            } while (offset > 0);

            return s;
        }
    }
}
