{-# LANGUAGE OverloadedStrings #-}

module TestRabbitmq where 


import Network.AMQP
import qualified Data.ByteString.Lazy.Char8 as BL


-- un type que je vais envoyer dans la queue rebbitmq

data Evenement = MessageEnvoye String deriving (Show,Eq,Read)

-- fonction qui construit un Evenement
mkEvent :: String -> Evenement
mkEvent str
    | null str  = MessageEnvoye "le contenu du message est vide"
    | otherwise = MessageEnvoye $ "voici le message envoye : " <> str

-- fonction envoi (publie) un Evenement (un message) a la queue
-- les queues utilisent le protocole AMQP pour functionner
-- 
sentToQueue :: String -> IO ()
sentToQueue x = do
    conn <- openConnection "localhost" "/" "guest" "guest"  -- on se connecte a 127.0.0.1 avec les credentials guest et guest
    chan <- openChannel conn

    -- declare a queue, exchange and binding
    res <- declareQueue chan newQueue {queueName = "testRabbitMq"}
    declareExchange chan newExchange {exchangeName = "myExchange", exchangeType = "direct"}
    bindQueue chan "testRabbitMq" "myExchange" "someKey"

     -- subscribe to the queue
    --consumeMsgs chan "testRabbitMq" Ack myCallback

     -- publish a message to our new exchange
    res1 <- publishMsg chan "myExchange" "someKey"
         newMsg {msgBody = (BL.pack $ show $ mkEvent x),
               msgDeliveryMode = Just Persistent}
    print res 
    print "\n"
    print res1
    closeConnection conn
    


-- function qui se connecte a un canal d'une queue pour recevoir les messages
-- c'est le subcriber de la queue testRabbitMq
-- ici j'ai utilisé les functions consumeMsgs et getMsg qui servent pour recuperer une donnée dans une queue
-- la libraire en fourni d'autres.

receiveFromQueue :: IO ()
receiveFromQueue = do
    conn <- openConnection "localhost" "/" "guest" "guest"  -- on se connecte a 127.0.0.1 avec les credentials guest et guest
    chan <- openChannel conn

    consumeMsgs chan "testRabbitMq" Ack myCallback
    -- issue <- getMsg chan Ack "testRabbitMq"
    -- case issue of
    --     Nothing    -> print "Nothing"
    --     Just (x,_) -> print x
    closeConnection conn


myCallback :: (Message,Envelope) -> IO ()
myCallback (msg, env) = do
    putStrLn $ "received message: " ++ (BL.unpack $ msgBody msg)
    -- acknowledge receiving the message
    ackEnv env





-- main :: IO ()
-- main = do
--     conn <- openConnection "localhost" "/" "guest" "guest"  -- on se connecte a 127.0.0.1 avec les credentials guest et guest
--     chan <- openChannel conn

--     -- declare a queue, exchange and binding
--     res <- declareQueue chan newQueue {queueName = "testRabbitMq"}
--     declareExchange chan newExchange {exchangeName = "myExchange", exchangeType = "direct"}
--     bindQueue chan "testRabbitMq" "myExchange" "someKey"
--    --print res

    -- -- subscribe to the queue
    -- consumeMsgs chan "myQueue" Ack myCallback

    -- -- publish a message to our new exchange
    -- publishMsg chan "myExchange" "myKey"
    --     newMsg {msgBody = (BL.pack "hello world"),
    --             msgDeliveryMode = Just Persistent}

    -- getLine -- wait for keypress
    -- closeConnection conn
    -- print res
    -- putStrLn "connection closed"


