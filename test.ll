; ModuleID = 'Extend'

%subrange = type { %range*, i32, i32, i32, i32 }
%range = type { i32, i32, %value*, %status*, %formula* }
%value = type { i8, i32, %subrange* }
%status = type opaque
%formula = type opaque
%string = type { i8*, i64, i32 }

@fmt = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%u\0A\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.3 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@0 = private unnamed_addr constant [12 x i8] c"Hello World\00"

define %subrange* @_main(%subrange*) {
entry:
  %malloccall = tail call i8* @malloc(i32 0)
  %_scope = bitcast i8* %malloccall to {}*
  %1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @0, i32 0, i32 0), i32 1)
  %malloccall1 = tail call i8* @malloc(i32 ptrtoint (%subrange* getelementptr (%subrange, %subrange* null, i32 1) to i32))
  %ret = bitcast i8* %malloccall1 to %subrange*
  ret %subrange* %ret
}

declare i32 @printf(i8*, ...)

declare i64 @strlen(i8*)

; Function Attrs: argmemonly nounwind
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture, i8* nocapture readonly, i64, i32, i1) #0

define i1 @is_subrange_1x1(%subrange*) {
entry:
  %the_pointer = getelementptr inbounds %subrange, %subrange* %0, i32 0, i32 3
  %the_value = load i32, i32* %the_pointer
  %the_bool = icmp eq i32 %the_value, 1
  %the_pointer1 = getelementptr inbounds %subrange, %subrange* %0, i32 0, i32 4
  %the_value2 = load i32, i32* %the_pointer1
  %the_bool3 = icmp eq i32 %the_value2, 1
  %one_by_one = and i1 %the_bool, %the_bool3
  ret i1 %one_by_one
}

define i32 @get_val(%range*, i32, i32) {
entry:
  ret i32 -1
}

define i32 @deref_subrange(%subrange*) {
entry:
  %the_base_range_ptr = getelementptr inbounds %subrange, %subrange* %0, i32 0, i32 0
  %the_base_range = load %range*, %range** %the_base_range_ptr
  %the_contents = call i32 @get_val(%range* %the_base_range, i32 0, i32 0)
  ret i32 %the_contents
}

define %string* @new_string(i8*) {
entry:
  %malloccall = tail call i8* @malloc(i32 ptrtoint (%string* getelementptr (%string, %string* null, i32 1) to i32))
  %the_string_ptr = bitcast i8* %malloccall to %string*
  %dst_char_ptr_ptr = getelementptr inbounds %string, %string* %the_string_ptr, i32 0, i32 0
  %string_len = call i64 @strlen(i8* %0)
  %extra_byte = add i64 %string_len, 1
  %strlen_ptr = getelementptr inbounds %string, %string* %the_string_ptr, i32 0, i32 1
  %strlen_ptr1 = getelementptr inbounds %string, %string* %the_string_ptr, i32 0, i32 2
  %1 = trunc i64 %extra_byte to i32
  %mallocsize = mul i32 %1, ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i32)
  %dst_char_ptr = tail call i8* @malloc(i32 %mallocsize)
  store i8* %dst_char_ptr, i8** %dst_char_ptr_ptr
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %dst_char_ptr, i8* %0, i64 %extra_byte, i32 0, i1 false)
  store i64 %string_len, i64* %strlen_ptr
  store i32 1, i32* %strlen_ptr1
  ret %string* %the_string_ptr
}

declare noalias i8* @malloc(i32)

define i32 @main(i32, i8**) {
entry:
  %input_arg = alloca %subrange
  %2 = call %subrange* @_main(%subrange* %input_arg)
  %argv_1_addr = getelementptr inbounds i8*, i8** %1, i32 1
  %argv_1 = load i8*, i8** %argv_1_addr
  %len_of_argv_1 = call i64 @strlen(i8* %argv_1)
  %int_len_of_argv_1 = trunc i64 %len_of_argv_1 to i32
  %ns_ptr = call %string* @new_string(i8* %argv_1)
  %ns_charptr_ptr = getelementptr inbounds %string, %string* %ns_ptr, i32 0, i32 0
  %ns_charptr = load i8*, i8** %ns_charptr_ptr
  ret i32 %int_len_of_argv_1
}

attributes #0 = { argmemonly nounwind }

