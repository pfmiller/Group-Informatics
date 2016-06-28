-- --------------------------------------------------------------------------------
-- Routine DDL
-- --------------------------------------------------------------------------------
DELIMITER $$

CREATE DEFINER=`root`@`%` PROCEDURE `getTnetFormattedNetwork_all`()
beginDECLARE done INT DEFAULT 0;declare releaseInner DOUBLE;declare cur1 CURSOR FOR select releaseId from research_release;DECLARE CONTINUE HANDLER FOR NOT FOUND SET done = 1;OPEN cur1;REPEAT  FETCH cur1 INTO releaseInner;  IF NOT done THEN    call getTnetFormattedNetwork(releaseInner);  END IF;UNTIL done END REPEAT;close cur1;end