/*
  Warnings:

  - You are about to drop the `user` table. If the table is not empty, all the data it contains will be lost.

*/
-- DropForeignKey
ALTER TABLE `category` DROP FOREIGN KEY `category_creator_id_fkey`;

-- DropForeignKey
ALTER TABLE `customer_description` DROP FOREIGN KEY `customer_description_user_id_fkey`;

-- DropForeignKey
ALTER TABLE `event` DROP FOREIGN KEY `event_creator_id_fkey`;

-- DropForeignKey
ALTER TABLE `faq` DROP FOREIGN KEY `faq_user_id_fkey`;

-- DropForeignKey
ALTER TABLE `faq_description` DROP FOREIGN KEY `faq_description_userId_fkey`;

-- DropForeignKey
ALTER TABLE `user` DROP FOREIGN KEY `user_role_id_fkey`;

-- AlterTable
ALTER TABLE `event` ADD COLUMN `is_enable` BOOLEAN NOT NULL DEFAULT false;

-- AlterTable
ALTER TABLE `setting` MODIFY `value` LONGTEXT NOT NULL;

-- DropTable
DROP TABLE `user`;

-- CreateTable
CREATE TABLE `admin_user` (
    `id` INTEGER NOT NULL AUTO_INCREMENT,
    `name` VARCHAR(191) NOT NULL,
    `email` VARCHAR(191) NOT NULL,
    `password` VARCHAR(191) NOT NULL,
    `role_id` INTEGER NOT NULL,
    `is_deleted` BOOLEAN NOT NULL DEFAULT false,
    `created_at` TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    `updated_at` TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),

    UNIQUE INDEX `admin_user_email_key`(`email`),
    INDEX `admin_user_role_id_idx`(`role_id`),
    PRIMARY KEY (`id`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- AddForeignKey
ALTER TABLE `admin_user` ADD CONSTRAINT `admin_user_role_id_fkey` FOREIGN KEY (`role_id`) REFERENCES `role`(`id`) ON DELETE NO ACTION ON UPDATE NO ACTION;

-- AddForeignKey
ALTER TABLE `faq` ADD CONSTRAINT `faq_user_id_fkey` FOREIGN KEY (`user_id`) REFERENCES `admin_user`(`id`) ON DELETE NO ACTION ON UPDATE NO ACTION;

-- AddForeignKey
ALTER TABLE `faq_description` ADD CONSTRAINT `faq_description_userId_fkey` FOREIGN KEY (`userId`) REFERENCES `admin_user`(`id`) ON DELETE SET NULL ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE `customer_description` ADD CONSTRAINT `customer_description_user_id_fkey` FOREIGN KEY (`user_id`) REFERENCES `admin_user`(`id`) ON DELETE NO ACTION ON UPDATE NO ACTION;

-- AddForeignKey
ALTER TABLE `category` ADD CONSTRAINT `category_creator_id_fkey` FOREIGN KEY (`creator_id`) REFERENCES `admin_user`(`id`) ON DELETE NO ACTION ON UPDATE NO ACTION;

-- AddForeignKey
ALTER TABLE `event` ADD CONSTRAINT `event_creator_id_fkey` FOREIGN KEY (`creator_id`) REFERENCES `admin_user`(`id`) ON DELETE NO ACTION ON UPDATE NO ACTION;
