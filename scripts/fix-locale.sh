#!/bin/bash

# Source the locale environment settings
source "$(dirname "$0")/../.locale-env"

# Run the command passed as arguments
exec "$@"