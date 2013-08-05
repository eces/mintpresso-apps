# --- !Ups
CREATE TABLE `edges` (
    `no` bigint(20) NOT NULL PRIMARY KEY AUTO_INCREMENT,
    `owner` bigint(20) NOT NULL,
    `s` bigint(20) NOT NULL,
    `sType` bigint(20) NOT NULL,
    `v` varchar(200) NOT NULL,
    `o` bigint(20) NOT NULL,
    `oType` bigint(20) NOT NULL,
    `json` VARCHAR(10240) NOT NULL,
    `created` DATETIME(6) NOT NULL,
    `updated` DATETIME(6) NOT NULL
);
ALTER TABLE `edges` ADD INDEX ( `ownerId` );

CREATE TABLE `nodes` (
    `no` bigint(20) NOT NULL PRIMARY KEY AUTO_INCREMENT,
    `owner` bigint(20) NOT NULL,
    `id` varchar(1000) NOT NULL,
    `type` bigint(20) NOT NULL,
    `created` DATETIME(6) NOT NULL,
    `updated` DATETIME(6) NOT NULL,
    `referenced` DATETIME(6) NOT NULL,
    `json` varchar(10240) NOT NULL
);
ALTER TABLE `nodes` ADD INDEX ( `ownerId` );

CREATE TABLE `types` (
  `no` bigint(20) NOT NULL PRIMARY KEY AUTO_INCREMENT,
  `name` varchar(200) NOT NULL UNIQUE
);

# --- !Downs
DROP TABLE IF EXISTS `edges`;
DROP TABLE IF EXISTS `points`;
DROP TABLE IF EXISTS `nodes`;
DROP TABLE IF EXISTS `pointTypes`;