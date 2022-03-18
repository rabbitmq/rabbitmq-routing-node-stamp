# Creating a release

* Update code on branch, test, etc
* Merge to `master`
* Tag release
    ```
    git tag -a -s -u B1B82CC0CF84BA70147EBD05D99DE30E43EAE440 -m 'rabbitmq-routing-node-stamp v1.0.0' 'v1.0.0'

    # If build metadata is necessary
    git tag -a -s -u B1B82CC0CF84BA70147EBD05D99DE30E43EAE440 -m 'rabbitmq-routing-node-stamp v1.0.0+rmq-39' 'v1.0.0+rmq-39'
    
    git push --tags
    ```
* Build release `.ez`:
    ```
    # Ensure Erlang and Elixir versions correspond with _minimum_ supported
    # for the RabbitMQ release version
    asdf local erlang 23.2
    asdf local elixir 1.11.4-otp-23
    make PROJECT_VERSION='1.0.0' DIST_AS_EZS=1 FULL=1 dist

    # If build metadata is necessary
    make PROJECT_VERSION='1.0.0+rmq-39' DIST_AS_EZS=1 FULL=1 dist
    ```
* Upload generated `*.ez` to GitHub release page.
