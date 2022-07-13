const flureenjs = require("@fluree/flureenjs");

test('expect all flureenjs functions to be defined', () => {
  expect(Object.keys(flureenjs).sort()).toStrictEqual([
      "_app_state",
      "_db_instance",
      "_generate_key_pair",
      "_init_state",
      "_njs_crypto",
      "_ns_state",
      "accountId",
      "blockEventToMap",
      "blockQuery",
      "blockRange",
      "blockRangeWithTxn",
      "close",
      "closeListener",
      "collectionFlakes",
      "collectionId",
      "connect",
      "db",
      "deleteLedger",
      "forwardTimeTravel",
      "graphql",
      "historyQuery",
      "httpSignature",
      "isForwardTimeTravelDb",
      "jldCommit",
      "jldConnect",
      "jldCreate",
      "jldDb",
      "jldLoad",
      "jldQuery",
      "jldStage",
      "jldStatus",
      "ledgerInfo",
      "ledgerList",
      "listen",
      "listeners",
      "monitorTx",
      "multiQuery",
      "newLedger",
      "newPrivateKey",
      "passwordGenerate",
      "passwordLogin",
      "predicateId",
      "predicateName",
      "publicKey",
      "publicKeyFromPrivate",
      "query",
      "queryWith",
      "renewToken",
      "resolveLedger",
      "search",
      "session",
      "setDefaultKey",
      "setLogging",
      "sign",
      "sparql",
      "sql",
      "subid",
      "transact",
      "txToCommand",
    ]);
})
