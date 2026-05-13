package webservice.rest;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.stereotype.Service;
import webservice.rest.dtos.GswbBatchOutput;

import java.io.IOException;
import java.net.URI;
import java.net.URLEncoder;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.HashMap;
import java.util.logging.Logger;

@Service
public class GswbRedisSessionService {

    private static final String DEFAULT_SESSION_KEY = "last_session";
    private static final Logger LOGGER = Logger.getLogger(GswbRedisSessionService.class.getName());

    private final ObjectMapper objectMapper;
    private final HttpClient httpClient;
    private final String apiBaseUrl;

    public GswbRedisSessionService(ObjectMapper objectMapper) {
        this.objectMapper = objectMapper;
        this.httpClient = HttpClient.newBuilder()
                .connectTimeout(Duration.ofSeconds(5))
                .build();
        String configuredBaseUrl = System.getenv().getOrDefault("REDIS_API_URL", "http://localhost:8083");
        this.apiBaseUrl = configuredBaseUrl.isBlank() ? "http://localhost:8083" : configuredBaseUrl;
    }

    public void saveBatchOutput(String sessionKey, GswbBatchOutput output) {
        try {
            callJson("/gswb_batch_session/" + encode(sessionKey), "PUT", output);
        } catch (Exception e) {
            LOGGER.warning("Unable to persist GSWB session snapshot: " + e.getMessage());
        }
    }

    public GswbBatchOutput loadBatchOutput(String sessionKey) {
        try {
            String body = callText("/gswb_batch_session/" + encode(sessionKey), "GET", null);
            if (body == null || body.isBlank()) {
                return new GswbBatchOutput(new HashMap<>(), "");
            }
            return objectMapper.readValue(body, GswbBatchOutput.class);
        } catch (IOException | InterruptedException e) {
            return new GswbBatchOutput(new HashMap<>(), "");
        }
    }

    public HashMap<String, Object> summarizeBatchOutput(String sessionKey) {
        try {
            String body = callText("/gswb_batch_session/" + encode(sessionKey) + "/summary", "GET", null);
            if (body == null || body.isBlank()) {
                return emptySummary(sessionKey);
            }
            return objectMapper.readValue(body, new TypeReference<HashMap<String, Object>>() {});
        } catch (IOException | InterruptedException e) {
            return emptySummary(sessionKey);
        }
    }

    public void clear(String sessionKey) {
        try {
            callText("/gswb_batch_session/" + encode(sessionKey), "DELETE", null);
        } catch (IOException | InterruptedException e) {
            LOGGER.warning("Unable to clear GSWB session: " + e.getMessage());
        }
    }

    private HashMap<String, Object> emptySummary(String sessionKey) {
        HashMap<String, Object> summary = new HashMap<>();
        summary.put("sessionKey", sessionKey);
        summary.put("item_count", 0);
        summary.put("report_size", 0);
        summary.put("updatedAt", "");
        return summary;
    }

    private String callText(String path, String method, Object payload) throws IOException, InterruptedException {
        try {
            HttpRequest request = buildRequest(path, method, payload);
            HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());
            int status = response.statusCode();
            if (status >= 200 && status < 300) {
                return response.body();
            }
            throw new IOException("HTTP " + status + " calling " + path);
        } catch (RuntimeException e) {
            throw new IOException("Unable to call " + path, e);
        }
    }

    private void callJson(String path, String method, Object payload) {
        try {
            callText(path, method, payload);
        } catch (IOException | InterruptedException e) {
            throw new IllegalStateException("Unable to persist GSWB session", e);
        }
    }

    private HttpRequest buildRequest(String path, String method, Object payload) throws IOException {
        HttpRequest.Builder builder = HttpRequest.newBuilder()
                .uri(URI.create(apiBaseUrl + path))
                .timeout(Duration.ofSeconds(10))
                .method(method, payload == null
                        ? HttpRequest.BodyPublishers.noBody()
                        : HttpRequest.BodyPublishers.ofString(objectMapper.writeValueAsString(payload)))
                .header("Accept", "application/json");

        if (payload != null) {
            builder.header("Content-Type", "application/json");
        }

        return builder.build();
    }

    private String encode(String sessionKey) {
        String key = (sessionKey == null || sessionKey.isBlank()) ? DEFAULT_SESSION_KEY : sessionKey;
        return URLEncoder.encode(key, StandardCharsets.UTF_8);
    }
}
