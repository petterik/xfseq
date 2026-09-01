package xfseq;

/**
 * Closed operation profiles used by the public unary compatibility boundary.
 *
 * <p>This is deliberately not a public policy switch.  The Clojure wrappers
 * select one profile when constructing a result; generic {@code xf-seq}
 * continues to use {@link #GENERIC}.</p>
 */
public enum UnaryProfile {
    GENERIC,
    MAP_LIKE,
    FILTER_LIKE,
    TAKE
}
