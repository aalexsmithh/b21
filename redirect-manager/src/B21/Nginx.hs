module B21.Nginx ( checkNginx, reloadNginx ) where

import System.Process ( callCommand )

-- | Runs @nginx -t@ to check the syntax of the nginx configuration files.
-- Throws an exception if the syntax is invalid.
checkNginx :: IO ()
checkNginx = callCommand "sudo nginx -t"

reloadNginx :: IO ()
reloadNginx = callCommand "sudo systemctl restart nginx"
