# Cloudy

Cloudy is a CLI tool to easily spin up and control compute instances in
various cloud environments.  You can think of it as an alternative to
cloud-specific CLI tools, like `aws` for AWS, or `scw` for Scaleway.

Cloudy focuses on easily controlling compute instances (as opposed to the full
functionality provided by cloud vendors).  Cloudy's goal is to make it easy to
run one-off cloud-based compute instances from the command line.  It is
somewhat similar to `docker`, but for running instances in the Cloud instead of
locally.

Cloudy is a good fit for things like the following:

- Spinning up a webserver in the cloud in order to show a friend or coworker a tech demo.
- Creating a temporary VPN in order to work around location restrictions.
- Creating a cloud-based environment to test a tool that you don't want to install locally.

Cloudy makes both spinning up and destroying an instance just a single command,
as well as providing nice Bash completion.  It is quite lightweight and quick
to use.

## Supported Cloud Providers

Cloudy currently supports the following cloud providers:

- [AWS](https://aws.amazon.com/) (support planned)
- [Scaleway](https://www.scaleway.com/)

PRs providing support for additional cloud providers are always welcome!

## Installation

Cloudy provides a single CLI tool, `cloudy`.

This section lists installation instructions for various distros.

### Generic Linux
