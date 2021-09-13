import scalaKanren.*

def a_and_b(): Goal = {
	conj(
		call_fresh(
			(a) => {
				eq(
					a,
					7
				)
			}
		),
		call_fresh(
			(b) => {
				disj(
					eq(
						b,
						5
					),
					eq(
						b,
						6
					)
				)
			}
		)
	)
}

def fives(x: Value): Goal = {
	disj(
		eq(
			x,
			5
		),
		(a_c) => {
			ImmatureStream(
				() => {
					((fives)(
						x
					))(
						a_c
					)
				}
			)
		}
	)
}

def appendo(l: Value, s: Value, out: Value): Goal = {
	disj(
		conj(
			eq(
				Null(),
				l
			),
			eq(
				s,
				out
			)
		),
		call_fresh(
			(a) => {
				call_fresh(
					(d) => {
						conj(
							eq(
								Cons(
									a,
									d
								),
								l
							),
							call_fresh(
								(res) => {
									conj(
										eq(
											Cons(
												a,
												res
											),
											out
										),
										(s_c) => {
											ImmatureStream(
												() => {
													((appendo)(
														d,
														s,
														res
													))(
														s_c
													)
												}
											)
										}
									)
								}
							)
						)
					}
				)
			}
		)
	)
}

def appendo2(l: Value, s: Value, out: Value): Goal = {
	disj(
		conj(
			eq(
				Null(),
				l
			),
			eq(
				s,
				out
			)
		),
		call_fresh(
			(a) => {
				call_fresh(
					(d) => {
						conj(
							eq(
								Cons(
									a,
									d
								),
								l
							),
							call_fresh(
								(res) => {
									conj(
										(s_c) => {
											ImmatureStream(
												() => {
													((appendo2)(
														d,
														s,
														res
													))(
														s_c
													)
												}
											)
										},
										eq(
											Cons(
												a,
												res
											),
											out
										)
									)
								}
							)
						)
					}
				)
			}
		)
	)
}

def call_appendo(): Goal = {
	call_fresh(
		(q) => {
			call_fresh(
				(l) => {
					call_fresh(
						(s) => {
							call_fresh(
								(out) => {
									conj(
										(appendo)(
											l,
											s,
											out
										),
										eq(
											Cons(
												l,
												Cons(
													s,
													Cons(
														out,
														Null()
													)
												)
											),
											q
										)
									)
								}
							)
						}
					)
				}
			)
		}
	)
}

def call_appendo2(): Goal = {
	call_fresh(
		(q) => {
			call_fresh(
				(l) => {
					call_fresh(
						(s) => {
							call_fresh(
								(out) => {
									conj(
										(appendo2)(
											l,
											s,
											out
										),
										eq(
											Cons(
												l,
												Cons(
													s,
													Cons(
														out,
														Null()
													)
												)
											),
											q
										)
									)
								}
							)
						}
					)
				}
			)
		}
	)
}

def call_appendo3(): Goal = {
	call_fresh(
		(q) => {
			call_fresh(
				(l) => {
					call_fresh(
						(s) => {
							call_fresh(
								(out) => {
									conj(
										eq(
											Cons(
												l,
												Cons(
													s,
													Cons(
														out,
														Null()
													)
												)
											),
											q
										),
										(appendo)(
											l,
											s,
											out
										)
									)
								}
							)
						}
					)
				}
			)
		}
	)
}

def ground_appendo(): Goal = {
	(appendo)(
		Cons(
			"a",
			Null()
		),
		Cons(
			"b",
			Null()
		),
		Cons(
			"a",
			Cons(
				"b",
				Null()
			)
		)
	)
}

def ground_appendo2(): Goal = {
	(appendo2)(
		Cons(
			"a",
			Null()
		),
		Cons(
			"b",
			Null()
		),
		Cons(
			"a",
			Cons(
				"b",
				Null()
			)
		)
	)
}

def relo(x: Value): Goal = {
	call_fresh(
		(x1) => {
			call_fresh(
				(x2) => {
					conj(
						eq(
							x,
							Cons(
								x1,
								x2
							)
						),
						disj(
							eq(
								x1,
								x2
							),
							(s_c) => {
								ImmatureStream(
									() => {
										((relo)(
											x
										))(
											s_c
										)
									}
								)
							}
						)
					)
				}
			)
		}
	)
}

def many_non_ans(): Goal = {
	call_fresh(
		(x) => {
			disj(
				(relo)(
					Cons(
						5,
						6
					)
				),
				eq(
					x,
					3
				)
			)
		}
	)
}
