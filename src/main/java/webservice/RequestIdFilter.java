package webservice;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.slf4j.MDC;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.util.concurrent.ThreadLocalRandom;
import java.util.regex.Pattern;

/**
 * Tags every log line of a request with a short id and the endpoint, e.g. " [3fa91c /deduce]"
 * (logback-spring.xml prints MDC key {@value #MDC_TAG} after the level). Requests run
 * concurrently, so without it their lines interleave and a message like "Now parsing input
 * premises..." cannot be told apart. An id arriving in {@value #HEADER} is kept, so a call from
 * GSWB to LiGER carries the caller's id into LiGER's log; the id is echoed in the response.
 */
@Component
@Order(Ordered.HIGHEST_PRECEDENCE)
public class RequestIdFilter extends OncePerRequestFilter {

    public static final String HEADER = "X-Request-Id";
    /** The bare id, for forwarding to other services. */
    public static final String MDC_ID = "reqId";
    /** What the log pattern prints. */
    public static final String MDC_TAG = "req";

    private static final Pattern VALID_ID = Pattern.compile("[\\w.-]{1,40}");

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response,
                                    FilterChain chain) throws ServletException, IOException {
        String incoming = request.getHeader(HEADER);
        String id = incoming != null && VALID_ID.matcher(incoming).matches()
                ? incoming
                : String.format("%06x", ThreadLocalRandom.current().nextInt(1 << 24));
        MDC.put(MDC_ID, id);
        MDC.put(MDC_TAG, " [" + id + " " + request.getRequestURI() + "]");
        response.setHeader(HEADER, id);
        try {
            chain.doFilter(request, response);
        } finally {
            MDC.remove(MDC_ID);
            MDC.remove(MDC_TAG);
        }
    }

    /** Adds an item (e.g. a sentence id) to the current tag, " [3fa91c /batch S12]", until closed:
     *  in a batch, which item a line belongs to matters as much as which request. */
    public static ItemScope item(String itemId) {
        return new ItemScope(itemId);
    }

    public static final class ItemScope implements AutoCloseable {
        private final String previous;

        private ItemScope(String itemId) {
            previous = MDC.get(MDC_TAG);
            if (previous != null && previous.endsWith("]")) {
                MDC.put(MDC_TAG, previous.substring(0, previous.length() - 1) + " " + itemId + "]");
            }
        }

        @Override
        public void close() {
            if (previous != null) {
                MDC.put(MDC_TAG, previous);
            }
        }
    }
}
