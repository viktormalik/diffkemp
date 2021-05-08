define void @skb_ext_put(%struct.sk_buff*) #3 {
  %2 = getelementptr inbounds %struct.sk_buff, %struct.sk_buff* %0, i32 0, i32 15
  %3 = load i8, i8* %2, align 1
  %4 = icmp ne i8 %3, 0
  br i1 %4, label %5, label %8

5:                                                ; preds = %1
  %6 = getelementptr inbounds %struct.sk_buff, %struct.sk_buff* %0, i32 0, i32 48
  %7 = load %struct.skb_ext*, %struct.skb_ext** %6, align 8
  call void @__skb_ext_put(%struct.skb_ext* %7)
  br label %8

8:                                                ; preds = %5, %1
  ret void
}
