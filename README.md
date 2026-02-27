# abacus-registers

Abacus provides a register interface that statically prevents 
device protocol violations, i.e. software initiated hardware operations
that violate the hardware's specification.

Abacus provides a DSL for developers to encode the hardware state machine
and device protocol. Abacus then autogenerates type-states and type-state 
transitions to represent the device protocol in the type-system and, using
the Rust compiler, statically prevent device protocol violations.

## Using abacus-registers

`abacus-registers` is a wrapper around the [`tock-registers`](https://crates.io/crates/tock-registers) interface. To use
Abacus, we first annotate the register `struct` as follows:

```Rust
#[repr(C)]
#[process_register_block(
    peripheral_name = "Nrf5xTemp",
    register_base_addr = 0x4000C000,
    states = [
        (Off),
        (Reading),
    ]
)]
struct TemperatureRegisters {
    /// Start temperature measurement
    /// Address: 0x000 - 0x004
    #[RegAttributes([Off], StateChange(Reading, [Task::ENABLE::SET]))]
    pub task_start: WriteOnly<u32, Task::Register>,
    /// Stop temperature measurement
    /// Address: 0x004 - 0x008
    #[RegAttributes([Reading], StateChange(Off, [Task::ENABLE::SET]))]
    pub task_stop: WriteOnly<u32, Task::Register>,
    /// Enable interrupt
    /// Address: 0x304 - 0x308
    pub intenset: ReadWrite<u32, Intenset::Register>,
    /// Disable interrupt
    /// Address: 0x308 - 0x30c
    pub intenclr: ReadWrite<u32, Intenclr::Register>,
    /// Temperature in °C (0.25° steps)
    /// Address: 0x508 - 0x50c
    #[RegAttributes([Reading], ReadOnly)]
    pub temp: ReadOnly<u32, Temperature::Register>,
```

Abacus currently supports annotating:
- State transitioning registers
- Constraining register operations to a set of states.

The `task_start` and `task_stop` registers above are examples of a state transitioning
register. 
```Rust
#[RegAttributes([Off], StateChange(Reading, [Task::ENABLE::SET]))]
                 ^^^                ^^^^^^         ^^^^^^^^^^
[States trans. valid from]    [State trans. to]   [Register bitfields to write to perform transition] 
```

The `temp` register represents a standard R/O register that may only be interacted
while the peripheral is in a certain state.
```Rust
#[RegAttributes([Reading], Readonly)]
pub temp: ReadOnly<u32, Temperature::Register>
```


A key difference with Abacus is that the driver no longer holds a reference to the 
register struct. 

```Rust
struct TemperatureDriver {
(-)  regs: &TemperatureRegisters
(+)  regs: AbacusCell(TemperatureStore)     
     ...
}
```

Instead, Abacus has a custom Cell type: `AbacusCell`. Because Abacus must track the hardware type-state
and must model state transitions, we use move semantics and a singleton type-state. The AbacusCell
uses interior mutability and enables `&self` drivers to provide access to an owned register type-state
within a closure.

```Rust
    fn read_temperature(&self) -> Result<(), ErrorCode> {
        self.registers
            .map(|regs| match regs {
                Nrf5xTempStore::Off(reg) => {
                    self.enable_interrupts(&reg);
                    reg.event_datardy.write(Event::READY::CLEAR);
                    let reg = reg.into_reading();
                    (reg.into(), Ok(()))
                }
                Nrf5xTempStore::Reading(reg) => (reg.into(), Err(ErrorCode::BUSY)),
            })
            .map_or_else(|| Err(ErrorCode::FAIL), |res| res)
    }
```

(see [this branch](https://github.com/abacus-mmio/tock/blob/abacus-mmio/chips/nrf5x/src/temperature.rs) for the full 
example shown above)
