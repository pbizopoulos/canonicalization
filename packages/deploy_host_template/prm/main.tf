terraform {
  required_version = ">= 1.0"
  required_providers {
    hcloud = {
      source  = "hetznercloud/hcloud"
      version = "~> 1.54"
    }
  }
  backend "local" {
    path = "../tmp/terraform.tfstate"
  }
}
variable "hcloud_token" {
  type      = string
  sensitive = true
}
variable "nixos_configuration_name" {
  type    = string
  default = "template"
}
variable "server_name" {
  type    = string
  default = "template"
}
variable "output_directory" {
  type = string
}
variable "hcloud_ssh_key_name" {
  type    = string
  default = "default"
}
provider "hcloud" {
  token = var.hcloud_token
}
data "hcloud_ssh_key" "default" {
  name = var.hcloud_ssh_key_name
}
resource "hcloud_server" "default" {
  name               = var.server_name
  server_type        = "cpx22"
  image              = "ubuntu-24.04"
  delete_protection  = true
  rebuild_protection = true
  ssh_keys = [
    data.hcloud_ssh_key.default.id
  ]
}
module "deploy" {
  source                 = "github.com/nix-community/nixos-anywhere//terraform/all-in-one"
  nixos_system_attr      = ".#nixosConfigurations.${var.nixos_configuration_name}.config.system.build.toplevel"
  nixos_partitioner_attr = ".#nixosConfigurations.${var.nixos_configuration_name}.config.system.build.diskoScript"
  target_host            = hcloud_server.default.ipv4_address
}
resource "local_file" "ipv4_address" {
  content  = hcloud_server.default.ipv4_address
  filename = "${var.output_directory}/ipv4_address"
}
