package webservice.rest;

import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

@Service
public class GswbService {
    private static RestTemplate restTemplate = new RestTemplate();
}
