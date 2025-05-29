# Makefile for pulpulak_series deployment

# Configuration
REMOTE_HOST = aladdin@103.13.211.36
REMOTE_PATH = /home/aladdin/pulpulak_series
ARCHIVE_NAME = pulpulak_series.tar.gz
LOCAL_ARCHIVE = ./$(ARCHIVE_NAME)

# Default target
.PHONY: help
help:
	@echo "Available targets:"
	@echo "  dist     - Create archive, deploy to remote server and start app"
	@echo "  archive  - Create gzipped archive of the repository"
	@echo "  deploy   - Send archive to remote server"
	@echo "  start    - Start the application on remote server"
	@echo "  clean    - Remove local archive"

# Create gzipped archive of the repository
.PHONY: archive
archive:
	@echo "Creating archive..."
	@tar --exclude='.git' \
		--exclude='node_modules' \
		--exclude='coverage' \
		--exclude='*.log' \
		--exclude='.DS_Store' \
		--exclude='$(ARCHIVE_NAME)' \
		-czf $(LOCAL_ARCHIVE) .
	@echo "Archive created: $(LOCAL_ARCHIVE)"

# Send archive to remote server, unpack and start app
.PHONY: dist
dist: archive deploy start

# Deploy archive to remote server
.PHONY: deploy
deploy:
	@echo "Sending archive to remote server..."
	@scp $(LOCAL_ARCHIVE) $(REMOTE_HOST):~/$(ARCHIVE_NAME)
	@echo "Unpacking archive on remote server..."
	@ssh $(REMOTE_HOST) "\
		mkdir -p $(REMOTE_PATH) && \
		cd $(REMOTE_PATH) && \
		tar -xzf ~/$(ARCHIVE_NAME) && \
		rm ~/$(ARCHIVE_NAME) && \
		echo 'Archive unpacked successfully'"

# Start the application on remote server
.PHONY: start
start:
	@echo "Installing dependencies and starting application..."
	@ssh $(REMOTE_HOST) "\
		cd $(REMOTE_PATH) && \
		npm install && \
		echo 'Dependencies installed' && \
		pkill -f 'node server.js' || true && \
		echo 'Starting application...' && \
		nohup npm start > app.log 2>&1 & \
		echo 'Application started in background' && \
		sleep 2 && \
		echo 'Application logs:' && \
		tail -n 10 app.log"

# Stop the application on remote server
.PHONY: stop
stop:
	@echo "Stopping application on remote server..."
	@ssh $(REMOTE_HOST) "\
		cd $(REMOTE_PATH) && \
		pkill -f 'node server.js' && \
		echo 'Application stopped' || echo 'No application running'"

# Check application status on remote server
.PHONY: status
status:
	@echo "Checking application status..."
	@ssh $(REMOTE_HOST) "\
		cd $(REMOTE_PATH) && \
		if pgrep -f 'node server.js' > /dev/null; then \
			echo 'Application is running (PID: '\$$(pgrep -f 'node server.js')')'; \
			echo 'Recent logs:'; \
			tail -n 5 app.log 2>/dev/null || echo 'No logs available'; \
		else \
			echo 'Application is not running'; \
		fi"

# View logs from remote server
.PHONY: logs
logs:
	@echo "Fetching application logs..."
	@ssh $(REMOTE_HOST) "\
		cd $(REMOTE_PATH) && \
		tail -n 50 app.log 2>/dev/null || echo 'No logs available'"

# Clean up local files
.PHONY: clean
clean:
	@echo "Cleaning up local archive..."
	@rm -f $(LOCAL_ARCHIVE)
	@echo "Cleanup complete"

# Full deployment with testing
.PHONY: deploy-test
deploy-test: dist
	@echo "Testing deployment..."
	@sleep 5
	@$(MAKE) status