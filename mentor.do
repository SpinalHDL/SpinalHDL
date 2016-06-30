add wave -noupdate  -group toplevel  -group  -label a /toplevel/a
add wave -noupdate  -group toplevel  -group  -label b /toplevel/b
add wave -noupdate  -group toplevel  -group  -label result /toplevel/result
add wave -noupdate  -group toplevel  -group   -group cmd -label valid /toplevel/cmd_valid
add wave -noupdate  -group toplevel  -group   -group cmd -label ready /toplevel/cmd_ready
add wave -noupdate  -group toplevel  -group   -group cmd  -group payload -label e1 /toplevel/cmd_payload_e1
add wave -noupdate  -group toplevel  -group   -group cmd  -group payload -label e2 /toplevel/cmd_payload_e2
add wave -noupdate  -group toplevel  -group   -group rsp -label valid /toplevel/rsp_valid
add wave -noupdate  -group toplevel  -group   -group rsp -label ready /toplevel/rsp_ready
add wave -noupdate  -group toplevel  -group   -group rsp  -group payload -label e1 /toplevel/rsp_payload_e1
add wave -noupdate  -group toplevel  -group   -group rsp  -group payload -label e2 /toplevel/rsp_payload_e2
add wave -noupdate  -group toplevel  -group fifo  -group io  -group push -label valid /toplevel/fifo/io_push_valid
add wave -noupdate  -group toplevel  -group fifo  -group io  -group push -label ready /toplevel/fifo/io_push_ready
add wave -noupdate  -group toplevel  -group fifo  -group io  -group push  -group payload -label e1 /toplevel/fifo/io_push_payload_e1
add wave -noupdate  -group toplevel  -group fifo  -group io  -group push  -group payload -label e2 /toplevel/fifo/io_push_payload_e2
add wave -noupdate  -group toplevel  -group fifo  -group io  -group pop -label valid /toplevel/fifo/io_pop_valid
add wave -noupdate  -group toplevel  -group fifo  -group io  -group pop -label ready /toplevel/fifo/io_pop_ready
add wave -noupdate  -group toplevel  -group fifo  -group io  -group pop  -group payload -label e1 /toplevel/fifo/io_pop_payload_e1
add wave -noupdate  -group toplevel  -group fifo  -group io  -group pop  -group payload -label e2 /toplevel/fifo/io_pop_payload_e2
add wave -noupdate  -group toplevel  -group fifo  -group io -label flush /toplevel/fifo/io_flush
add wave -noupdate  -group toplevel  -group fifo  -group pushPtr -label willIncrement /toplevel/fifo/pushPtr_willIncrement
add wave -noupdate  -group toplevel  -group fifo  -group pushPtr -label willClear /toplevel/fifo/pushPtr_willClear
add wave -noupdate  -group toplevel  -group fifo  -group pushPtr -label valueNext /toplevel/fifo/pushPtr_valueNext
add wave -noupdate  -group toplevel  -group fifo  -group pushPtr -label value /toplevel/fifo/pushPtr_value
add wave -noupdate  -group toplevel  -group fifo  -group popPtr -label willIncrement /toplevel/fifo/popPtr_willIncrement
add wave -noupdate  -group toplevel  -group fifo  -group popPtr -label willClear /toplevel/fifo/popPtr_willClear
add wave -noupdate  -group toplevel  -group fifo  -group popPtr -label valueNext /toplevel/fifo/popPtr_valueNext
add wave -noupdate  -group toplevel  -group fifo  -group popPtr -label value /toplevel/fifo/popPtr_value
add wave -noupdate  -group toplevel  -group fifo -label ptrMatch /toplevel/fifo/ptrMatch
add wave -noupdate  -group toplevel  -group fifo -label risingOccupancy /toplevel/fifo/risingOccupancy
add wave -noupdate  -group toplevel  -group fifo -label pushing /toplevel/fifo/pushing
add wave -noupdate  -group toplevel  -group fifo -label popping /toplevel/fifo/popping
add wave -noupdate  -group toplevel  -group fifo -label empty /toplevel/fifo/empty
add wave -noupdate  -group toplevel  -group fifo -label full /toplevel/fifo/full
