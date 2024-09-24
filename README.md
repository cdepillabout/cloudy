# Cloudy

Cloudy is a CLI tool to easily spin up and control compute instances in
various cloud environments.  You can think of it as an alternative to
cloud-specific CLI tools, like `aws` for AWS, or `scw` for Scaleway.

Cloudy focuses on easily controlling compute instances (as opposed to the full
functionality provided by cloud vendors).  Cloudy's goal is to make it easy to
run one-off cloud-based compute instances from the command line.  It is
somewhat similar to `docker` or `vagrant`, but for running instances in the
Cloud instead of locally.

Cloudy is a good fit for things like the following:

- Spinning up a webserver in the cloud in order to show a friend or coworker a tech demo.
- Creating a temporary VPN in order to work around location restrictions.
- Creating a cloud-based environment to test a tool that you don't want to install locally.

Cloudy makes both spinning up and destroying an instance just a single command,
as well as providing nice Bash completion.  It is quite lightweight and quick
to use.

For instance, creating a new instance on Scaleway with a 150 gb disk is as
simple as:

```console
$ cloudy scaleway create --volume-size 150
```

You can then SSH to the instance with the command:

```console
$ cloudy ssh
```

When you're finished, you can destroy the instance (and all related resources)
with the command:

```console
$ cloudy destroy
```

## Supported Cloud Providers

Cloudy currently supports the following cloud providers:

- [AWS](https://aws.amazon.com/) ([support planned](https://github.com/cdepillabout/cloudy/issues/2))
- [Scaleway](https://www.scaleway.com/)

PRs providing support for additional cloud providers are always welcome!

## Installation

Cloudy provides a single CLI tool, `cloudy`.

This section lists installation instructions for various distros.

### Generic Linux

There is a statically-linked x86_64 Linux ELF binary for `cloudy` available on each of
the [GitHub Releases](https://github.com/cdepillabout/cloudy/releases).

### Nix / NixOS

Cloudy can be built with Nix by using the `.nix` files in this repo:

```console
$ nix-build
```

You can find the `cloudy` binary in `./result/bin/cloudy`.

You can also install Cloudy from Nixpkgs by using the `haskellPackages.cloudy`
derivation:

```console
$ nix-build '<nixpkgs>' -A haskellPackages.cloudy
```

### Other Distros

PRs are welcome adding instructions for installing Cloudy on other Linux
distributions!

## Setup

Cloudy requires some setup before first use. The required setup is different
depending on which cloud providers you want to use.

You must create a config file in `~/.config/cloudy/cloudy.yaml`.  The following
sections explain what options need to be set in the config file for each cloud
provider.

### Setup for Scaleway

You need to add things like your Scaleway access key and secret key to the
`~/.config/cloudy/cloudy.yaml` config file.

You can find these values by generating a new API key on the Scaleway website
at <https://console.scaleway.com/iam/api-keys>:

```yaml
scaleway:
  # (required) Scaleway key, secret, and default org and project.  You can get these
  # values from the Scaleway website by generating a new API key.
  access_key: "SCWXIH85IR279KUH0X2I"
  secret_key: "4985bead-91a4-4e96-9e8e-5d88280cf26b"
  default_organization_id: "074712d9-c5e9-454c-b909-7a6be0c92cf0"
  default_project_id: "074712d9-c5e9-454c-b909-7a6be0c92cf0"

  # (optional) The default zone to use.  You can find all zones listed in
  # https://registry.terraform.io/providers/scaleway/scaleway/latest/docs/guides/regions_and_zones
  #
  # If you don't specify this, then you'll need to pass it on the command line
  # to all commands that require it.
  default_zone: "nl-ams-1"

  # (optional) The default instance type to use when creating instances.
  # You can find all instance types in the output of the
  # `cloudy scaleway list-instance-types` command.
  #
  # If you don't specify this, then you'll need to pass it on the command line
  # to all commands that require it.
  default_instance_type: "PLAY2-PICO"

  # (optional) The default image to use.  You can find other image IDs in the
  # output of the `cloudy scaleway list-images` command.
  #
  # If you don't specify this, then you'll need to pass it on the command line
  # to all commands that require it.
  default_image_id: "ubuntu_noble"
```

### Setup for AWS

(AWS support has [not yet been added](https://github.com/cdepillabout/cloudy/issues/2).)

## Usage

This section explains how to use Cloudy, assuming you've gone through the setup above.

### Create a Cloud Instance

The commands for creating a cloud instance differ between cloud providers.
The commands for each cloud provider are explained below.

Note that if you experience an error when running a command to create a cloud
instance, you must go to the cloud provider management UI and manually delete
all resources that Cloudy has created.

### Create a Scaleway Instance

A Scaleway instance can be created with a command like the following:

```console
$ cloudy scaleway create \
    --zone nl-ams-1 \
    --instance-type PLAY2-NANO \
    --volume-size 75 \
    --image-id "1ec31ce3-bec3-4866-81fc-08d4b6966f9f"
```

This creates a `PLAY2-NANO` Scaleway instance in zone `nl-ams-1` with a 75 GB
root disk, using an Ubuntu 24.04 disk image.

Here's the meaning of each of these arguments:

-   `--zone`: A Scaleway zone.  You can find all available values in
    <https://registry.terraform.io/providers/scaleway/scaleway/latest/docs/guides/regions_and_zones>.

-   `--instance-type`: The Scaleway instance type.  You can find all
    available instance types with the command
    `cloudy scaleway list-instance-types`.

-   `--volume-size`: The size in GB of the root disk.  Some instance
    types have minimum / maximum root disk sizes that they support.
    You can find this information in the output of the
    `cloudy scaleway list-instance-types` command.

    Cloudy provides no way to add additional disks to an instance.

-   `--image-id`: The ID of the image.  You can find all available
    image IDs with the command `cloudy scaleway list-images`.  There
    are images available for many popular Linux distros.

See `cloudy scaleway create --help` for more information.

### Create an AWS EC2 Instance

(AWS support has [not yet been added](https://github.com/cdepillabout/cloudy/issues/2).)

### List all Cloud Instances

You can see all your active Cloud Instances with the `cloudy list` command:

```console
$ cloudy list
|---------------------------------------------------------------------------------------------------------|
| instance id | instance name  |   created date   |  cloud   |   zone   |       ip       | instance setup |
|=============|================|==================|==========|==========|================|================|
|           7 | belief-example | 2024-09-22 18:23 | scaleway | nl-ams-1 | 61.252.131.123 | nginx          |
|          10 | land-secretary | 2024-09-22 18:24 | scaleway | fr-par-3 | 73.128.200.201 | (none)         |
|---------------------------------------------------------------------------------------------------------|
```

### SSH to Cloud Instance

You can SSH to an instance with the `cloudy ssh` command:

```console
$ cloudy ssh --name belief-example
```

You can pick which instance to SSH to with either the `--name` or `--id`
argument. If you have only a single instance available, then you don't need to
specify either the `--name` or `--id` argument.

### Copy Files to/from Cloud Instance

You can copy files to an instance with a command like the following:

```console
$ cloudy copy-file --to-instance ./my-local-file my-remote-file
```

This copies the local file `my-local-file` to the cloud instance with the name
`my-remote-file`.

See `cloudy copy-file --help` for more examples of copying files.

### Destroy Cloud Instance

You can destroy a cloud instance with a command like the following:

```console
$ cloudy destroy --name belief-example
```

You can pick which instance to SSH to with either the `--name` or `--id`
argument. If you have only a single instance available, then you don't need to
specify either the `--name` or `--id` argument.

After destroying the instance, you likely want to double check the cloud
provider's management UI to confirm that all the related resources have been
deleted.

## Instance Setup Scripts

One of the nicest features of Cloudy is the ability to specify Instance Setup
scripts. These are YAML files that contain cloud-init scripts to run when the
instance boots up.  Here's an example of what a Cloudy Instance Setup script
looks like:

`hello-world.yaml`:

```yaml
short-description: A basic hello-world instance-setup that shows how to run a bash script.

cloud-init-user-data: |
  #!/usr/bin/env bash

  echo "hello world from Cloudy!" > /tmp/hello-from-cloudy
```

The Bash code in `cloud-init-user-data` will be run when the instance first
boots up.

Cloudy provides some built-in Instance Setup scripts, as well as allowing the
user to define their own private scripts.

### Cloudy Built-in Instance Setup Scripts

You can see which instance setup scripts are available by either looking in the
[`instance-setups`](./instance-setups) directory in this repo, or looking at
the output of the `cloudy PROVIDER create --help` command.

For instance, with Scaleway:

```console
$ cloudy scaleway create --help
...
Available options:
  -t,--instance-setup INSTANCE_SETUP
                           Name of the instance-setup to use when booting the
                           image. (default: do no instance setup)
...
Default instance-setup scripts builtin to Cloudy:

    - hello-world  --  A basic hello-world instance-setup that shows how to run a bash script.
    - hello-world-cloud-config  --  A basic hello-world instance-setup that shows how to run a cloud-config script.
```

You can see here that there are two instance-setup scripts available,
`hello-world` and `hello-world-cloud-config`.

You can specify an instance setup script when creating a new instance.  For
instance, when creating a new instance on Scaleway:

```console
$ cloudy scaleway create --instance-setup hello-world
```

### Defining your own Instance Setup Script

You can define your own private Instance Setup scripts.

First, create a YAML file in `~/.config/cloudy/instance-setups/`.  Here's an
example file:

`foobar.yaml`:

```yaml
short-description: An example instance-setup script

cloud-init-user-data: |
  #cloud-config

  # Install these packages using the default package manager of the instance.
  packages:
    - cowsay

  # Run these commands after bootup.
  runcmd:
    - |
      cowsay "hello world from Cloudy!" > /tmp/hello-from-cloudy
```

There are a few required keys within an Instance Setup script:

- `short-description`: _[string]_ an explanation of what this instance-setup script does

- `cloud-init-user-data`: _[string]_ a cloud-init compatible user-data file.

    This is generally either a normal shell script that starts with a hash-bang line like
    `#!/usr/bin/env bash`, or a cloud-config script that starts with `#cloud-config`.

    See the [cloud-init documentation](https://cloudinit.readthedocs.io/en/latest/index.html),
    for specifics on how cloud-init works.

    You may be interested in the page on what cloud-init script
    [formats are available](https://cloudinit.readthedocs.io/en/latest/explanation/format.html),
    including normal shell scripts and cloud-config scripts.

## Moving Past Cloudy

Cloudy is meant to be simple and easy to use from the command line.  If you
want to change many additional settings when creating an instance, or you want
to create multiple instances at once, you likely want a more flexible solution
than Cloudy.

I recommend you look into either directly calling the CLI tool for a given
cloud provider, or using a tool like Terraform to create cloud resources.

## Versioning

Cloudy is versioned according to the [Haskell PVP](https://pvp.haskell.org/).

The Haskell PVP is very close to [Semver](https://semver.org/), except there is
an extra MAJOR version component.

For instance, with a Cloudy version like `MAJOR.MAJOR.MINOR.PATCH` (`1.2.3.4`), you can think of it
being similar to a Semver like `MAJOR.MINOR.PATCH` (`12.3.4`).

In the Haskell PVP, the leftmost `MAJOR` version number is only bumped to show
a _significant_ change in functionality, stableness, or usability.

## Releasing

Here are the steps to release a new version of Cloudy:

1.  Send and merge a PR bumping the version number according to the Haskell
    PVP, as well as adding a new [`CHANGELOG.md`](./CHANGELOG.md) entry.

2.  Go to the GitHub [Releases](https://github.com/cdepillabout/cloudy/releases)
    page and click "Draft a new release".

3.  In the "Choose a tag" drop-down, create a new tag with the format
    `v1.2.3.4` with the newly-bumped version number.  Use this same tag as the title.
    Copy and paste the new CHANGELOG entries to the release notes.
    Click "Publish release".

    This also creates a new Git tag, which you can fetch locally with a command like
    `git fetch --tags`.

4.  Upon publishing the release, a GitHub Action should run that builds and
    uploads a statically-linked `cloudy` binary as a Release Asset.

5.  Upload the new version of Cloudy to Hackage with a command like
    `stack upload .`

## WARNING

Cloudy is still beta software.  It is possible that cloud resources are not
correctly created or deleted.  You should make sure to manually check your
cloud provider's management UI to confirm that all expected resources have been
deleted after running commands like `cloudy destroy`.

You should also make sure to check the cloud provider's management UI if any
Cloudy commands fail, like `cloudy PROVIDER create`.

Cloudy is provided as-is, without any warranty of any kind.  You are solely
responsible for all charges incurred due to Cloudy usage, even in the face of
Cloudy-related bugs or mistakes.
