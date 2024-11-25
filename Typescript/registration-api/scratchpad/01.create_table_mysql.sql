CREATE TABLE `users` ( 
  `id` INT AUTO_INCREMENT NOT NULL,
  `email` VARCHAR(50) NOT NULL,
  `password` VARCHAR(250) NOT NULL,
   PRIMARY KEY (`id`),
  CONSTRAINT `users_unique` UNIQUE (`id`, `email`)
);
CREATE UNIQUE INDEX `users_index` 
ON `users` (
  `id` ASC
);