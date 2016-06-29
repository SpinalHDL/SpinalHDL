add wave -noupdate  -group toplevel  -group io /toplevel/io_a
add wave -noupdate  -group toplevel  -group io /toplevel/io_b
add wave -noupdate  -group toplevel  -group io /toplevel/io_result
add wave -noupdate  -group toplevel  -group io  -group io_cmd /toplevel/io_cmd_valid
add wave -noupdate  -group toplevel  -group io  -group io_cmd /toplevel/io_cmd_ready
add wave -noupdate  -group toplevel  -group io  -group io_cmd  -group io_cmd_payload /toplevel/io_cmd_payload_e1
add wave -noupdate  -group toplevel  -group io  -group io_cmd  -group io_cmd_payload /toplevel/io_cmd_payload_e2
add wave -noupdate  -group toplevel  -group io  -group io_rsp /toplevel/io_rsp_valid
add wave -noupdate  -group toplevel  -group io  -group io_rsp /toplevel/io_rsp_ready
add wave -noupdate  -group toplevel  -group io  -group io_rsp  -group io_rsp_payload /toplevel/io_rsp_payload_e1
add wave -noupdate  -group toplevel  -group io  -group io_rsp  -group io_rsp_payload /toplevel/io_rsp_payload_e2
add wave -noupdate  -group toplevel  -group fifo  -group io  -group io_push /toplevel/fifo/io_push_valid
add wave -noupdate  -group toplevel  -group fifo  -group io  -group io_push /toplevel/fifo/io_push_ready
add wave -noupdate  -group toplevel  -group fifo  -group io  -group io_push  -group io_push_payload /toplevel/fifo/io_push_payload_e1
add wave -noupdate  -group toplevel  -group fifo  -group io  -group io_push  -group io_push_payload /toplevel/fifo/io_push_payload_e2
add wave -noupdate  -group toplevel  -group fifo  -group io  -group io_pop /toplevel/fifo/io_pop_valid
add wave -noupdate  -group toplevel  -group fifo  -group io  -group io_pop /toplevel/fifo/io_pop_ready
add wave -noupdate  -group toplevel  -group fifo  -group io  -group io_pop  -group io_pop_payload /toplevel/fifo/io_pop_payload_e1
add wave -noupdate  -group toplevel  -group fifo  -group io  -group io_pop  -group io_pop_payload /toplevel/fifo/io_pop_payload_e2
add wave -noupdate  -group toplevel  -group fifo  -group io /toplevel/fifo/io_flush
add wave -noupdate  -group toplevel  -group fifo  -group pushPtr /toplevel/fifo/pushPtr_willIncrement
add wave -noupdate  -group toplevel  -group fifo  -group pushPtr /toplevel/fifo/pushPtr_willClear
add wave -noupdate  -group toplevel  -group fifo  -group pushPtr /toplevel/fifo/pushPtr_valueNext
add wave -noupdate  -group toplevel  -group fifo  -group pushPtr /toplevel/fifo/pushPtr_value
add wave -noupdate  -group toplevel  -group fifo  -group popPtr /toplevel/fifo/popPtr_willIncrement
add wave -noupdate  -group toplevel  -group fifo  -group popPtr /toplevel/fifo/popPtr_willClear
add wave -noupdate  -group toplevel  -group fifo  -group popPtr /toplevel/fifo/popPtr_valueNext
add wave -noupdate  -group toplevel  -group fifo  -group popPtr /toplevel/fifo/popPtr_value
add wave -noupdate  -group toplevel  -group fifo /toplevel/fifo/ptrMatch
add wave -noupdate  -group toplevel  -group fifo /toplevel/fifo/risingOccupancy
add wave -noupdate  -group toplevel  -group fifo /toplevel/fifo/pushing
add wave -noupdate  -group toplevel  -group fifo /toplevel/fifo/popping
add wave -noupdate  -group toplevel  -group fifo /toplevel/fifo/empty
add wave -noupdate  -group toplevel  -group fifo /toplevel/fifo/full
