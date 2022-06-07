---
CIP: TBD
Title: dApp events
Authors: sorki <srk@48.io>
Comments-Summary: No comments
Comments-URI:
Status: Draft
Type: Standards Track
Created: 2022-04-02
License: CC-BY-4.0
---

# dApp events

## Abstract

In this document we describe a communication standard
for dApp backends that allows dApps to react to specific
events happening on-chain. Such standard should help
dApp developers focus on core functionality of their
application instead of building their own supporting
functionality.

## Motivation

Since Cardano / Plutus dApps are eUTXO based, they
need to be able to receive notifications on specific
blockchain events like
* specified slot reached
* new UTXO produced at script address
* UTXO at script address spent
* transaction added to chain / transaction confirmed

Tracking of these events can be difficult, especially in presence of chain rollbacks.

## Specification

### Core data types

```hs
newtype Slot = Slot Integer
newtype Address = Address Text
newtype Tx = Tx Text
-- output reference of the transaction
type TxOutRef = (Tx, Integer)
type Validity = Bool
```

### `Event` types

#### `SlotReached Slot`

Produced when blockchain reaches a specific slot.

#### `UtxoSpent TxOutRef Tx`

Produced when a UTXO pointed to by `TxOutRef`
is spent by a transaction `Tx`.

#### `UtxoProduced Address [Tx]`

Produced when a new UTXO is created at an `Address`.
Event contains a list of transactions that produce
new outputs at this address.


#### `TransactionOnChain Tx Validity`

Produced when transaction `Tx` appears on-chain.


#### `AddressFundsChanged Address`

Produced when `Address` balance changes - either new UTXO
is produced at `Address` or an existing UTXO is spent.

This is supposed to notify dApp backend to refresh
the balance of the users or script address.

#### `Rollback Event`

Any of the events above wrapped in `Rollback` means
that the chain rollback occurred and a prior event
is no longer valid.

### Request types

Each event has a corresponding `Request` which dApp can
use to subscribe to specific `Event`s.

#### `SlotRequest Slot`

Await a slot.

#### `UtxoSpentRequest TxOutRef`

Get notified when UTXO at `TxOutRef` gets spent.

#### `UtxoProducedRequest Address`

Get notified when new UTXO appears at `Address`.

#### `TransactionStatusRequest Tx`

When transaction `Tx` appears on chain,
produce `TransactionTentative` event with current
number of confirmations. If confirmation count is
over the required confirmations `TransactionConfirmed`
event is produced.

#### `Recurring Request`

By default, each `Request` only produces one `Event`.
Recurring requests can be used with
`UtxoProducedRequest` and `TransactionStatusRequest`
to receive all `Event`s without having to created
requests repeatedly.

`TransactionStatusRequest` is removed when transaction
reaches a confirmed state.

### `Event` envelope

Each event is wrapped in an envelope data type
containing
* `id` - `id` of the `Request` that produced the `Event`
* `clientId` - `UUID` - unique client identifier
* `time` - POSIX Time indicating when the `Event` occurred
* `abs_slot` - Absolute slot number
* `block` - Absolute block number (block height)

### API

Subscription and retrieval of events is carried
out via `REST API` with all data encoded as `JSON`.

#### Client management

Implementation supports multiple clients (sessions)
identified by `UUID`s. Each client has a separate
set of subscriptions (requests) and a event queue.

##### `/clients/new`

`HTTP POST` request to this path triggers creation of a new client
identified by `UUID` which is returned by the endpoint as `JSON` string.

##### `/clients/remove/<UUID>`

`POST` request to this path deletes a client and all the stored events,
produced for it.

#### Request CRUD management

##### `/clients/request/<UUID>`

`POST` request creates a new request for this client.
Unique numeric `ID` of the request is returned as `JSON` number.

##### `/clients/events/<UUID>`

`GET` request allows retrieving all `Event`s produced
for this client based on set of its `Request`s.

## Rationale

### Design choices

#### Rest API

We choose to expose events and request management using simple
Rest API for interoperability with various consumers. Other event delivery methods like webhooks, websockets or server sent events may be supported by the implementation. All delivery methods use JSON encoding of the events.

#### Security factor

Configurable number of required confirmations until event
becomes visible in the events API endpoint. dApp can then choose appropriate factor to delay events until the probability
of a rollback is minimal. Typically, the chains see a rollbacks
of 1-2 blocks so security factor between 10-20 blocks should be considered. dApp may choose to use a factor of 0 to receive
all events instantly but has to deal with frequent rollbacks as well which could be useful for logging purposes.

#### Clients

Implementation supports multiple clients identified by unique identifier (UUID). This allows for multiple dApps to use a single instance.

#### Rollbacks

In presence of rollbacks that are larger than a security factor, we notify clients with a rollback event. We may hide rollbacks from clients that did not yet accessed the prior event which is getting invalidated by rollback and instead present only a new event when it occurs. This is achieved by tracking a pointer of last event accessed (seen) by the client.

## Initial implementation

* https://github.com/cardano-foundation/dab/
