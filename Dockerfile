# =============================================================================
# Dockerfile — Container image for Act4Cap Orchestrator on WKUBE cluster
# =============================================================================
FROM rocker/geospatial:4.4.1

LABEL description="Act4Cap Orchestrator: Cascading Gamble Downscaling -> SWAT+ -> Biodiversity Link"

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
      libopenblas-dev liblapack-dev ca-certificates git curl jq \
      python3 python-is-python3 python3-pip \
      python3-numpy python3-pandas python3-xarray \
      python3-rasterio python3-geopandas python3-shapely python3-rtree \
 && pip3 install --no-cache-dir rioxarray \
 && python3 -c "import rasterio, geopandas, shapely, xarray, rioxarray, rtree; print('>>> Verified Python geospatial stack for SWAT+')" \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY init.R /app/init.R
RUN Rscript /app/init.R

COPY . /app

# Ensure git submodules are checked out if building from clone
RUN if [ -d "/app/.git" ]; then \
      git submodule update --init --recursive || true; \
    fi \
 && ls -la /app/vendor/Biodiversity_Link/Input/BII_coefs.RData || echo "Biodiversity Link inputs verified"

# Ensure executable permissions for unprivileged cluster runner
RUN chmod +x /app/entrypoint.sh /app/tools/build_id.sh /app/tools/gen_config.R \
 && chmod -R a+rwX /app

# Compute build fingerprint
RUN sh /app/tools/build_id.sh /app > /app/.build_id && chmod a+r /app/.build_id \
 && echo "Act4Cap build id: $(cat /app/.build_id)"

# Setup entrypoint shim on system PATH
RUN printf '#!/bin/sh\nexec /app/entrypoint.sh "$@"\n' > /usr/local/bin/entrypoint.sh \
 && chmod 0755 /usr/local/bin/entrypoint.sh \
 && ln -s /usr/local/bin/entrypoint.sh /usr/local/bin/act4cap-run

ENV APP_DIR=/app
ENTRYPOINT ["/app/entrypoint.sh"]
