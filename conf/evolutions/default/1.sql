# --- !Ups
CREATE TABLE `edges` (
    `id` bigint(20) NOT NULL PRIMARY KEY AUTO_INCREMENT,
    `ownerId` bigint(20) NOT NULL,
    `sId` bigint(20) NOT NULL,
    `sType` bigint(20) NOT NULL,
    `v` varchar(200) NOT NULL,
    `oId` bigint(20) NOT NULL,
    `oType` bigint(20) NOT NULL,
    `data` VARCHAR(10240) NOT NULL,
    `createdAt` DATETIME(6) NOT NULL,
    `updatedAt` DATETIME(6) NOT NULL
);
ALTER TABLE `edges` ADD INDEX ( `ownerId` );

CREATE TABLE `points` (
    `id` bigint(20) NOT NULL PRIMARY KEY AUTO_INCREMENT,
    `ownerId` bigint(20) NOT NULL,
    `identifier` varchar(1000) NOT NULL,
    `typeId` bigint(20) NOT NULL,
    `createdAt` DATETIME(6) NOT NULL,
    `updatedAt` DATETIME(6) NOT NULL,
    `referencedAt` DATETIME(6) NOT NULL,
    `data` varchar(10240) NOT NULL
);
ALTER TABLE `points` ADD INDEX ( `ownerId` );

CREATE TABLE `pointTypes` (
  `id` bigint(20) NOT NULL PRIMARY KEY AUTO_INCREMENT,
  `name` varchar(200) NOT NULL UNIQUE
);

# --- !Downs
DROP TABLE IF EXISTS `edges`;
DROP TABLE IF EXISTS `points`;
DROP TABLE IF EXISTS `pointTypes`;