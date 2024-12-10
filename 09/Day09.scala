
object Utils09 {
  def openFile(path: String): List[String] = {
    val source = scala.io.Source.fromFile(path)
    val lines = source.getLines().toList
    lines
  }
}

case class File(id: Int, index: Int, size: Int):
    require(id != Disk.freeId)
end File

type Disk = Array[Int]

extension (d: Disk) def freeSpot(i: Int): Boolean = d(i) == -1
extension (d: Disk) def freeSpot(i: Int, size: Int): Boolean = (i until (i + size)).forall(i => d.freeSpot(i))
extension (d: Disk) def move(from: Int, to: Int): Unit = {
    require(from >= 0 && from < d.size)
    require(to >= 0 && to < d.size)
    require(d.freeSpot(to))
    val id = d(from)
    d(to) = id
    d(from) = Disk.freeId

}

extension (d: Disk) def moveFile(file: File, to: Int) = {
    require(file.index >= 0 && file.index < d.size)
    require(to >= 0 && to < d.size)
    require(to + file.size >= 0 && to + file.size < d.size)
    require(d.freeSpot(to, file.size))
    
    for (i <- (to until to + file.size)) do d(i) = file.id
    for (i <- (file.index until file.index + file.size)) do d(i) = Disk.freeId
}

extension (d: Disk) def isCompact: Boolean = 
    var compact = true
    var seenFree = false
    for (i <- (0 until d.size)) do 
        if seenFree && !d.freeSpot(i) then compact = false
        if d.freeSpot(i) then seenFree = true
    end for
    compact

extension (d: Disk) def compactDisk(): Unit = {
        var i = 0
        var j = d.length - 1
        while (j >= 0 && d.freeSpot(j)) do j -= 1
        assert(!d.freeSpot(j))
        while (i < j) do 
            
            if d.freeSpot(i) then 
                // println(f"current disk layout: ${disk.toTextRepr()}")
                d.move(j, i)
            i += 1
            while (j >= 0 && d.freeSpot(j)) do j -= 1
            assert(!d.freeSpot(j))
    }.ensuring(_ => d.isCompact)

extension (d: Disk) def toTextRepr(): String = {
    d.map(c => if c == Disk.freeId then "." else f"$c").mkString(" ")
}
extension (d: Disk) def checksum: Long = 
    d.zipWithIndex.map((fId, index) => if fId != Disk.freeId then fId.toLong * index.toLong else 0).sum

extension (d: Disk) def asFiles: List[File] = {
    var res: List[File] = Nil
    var i = 0 
    while (i < d.size) {
        if !d.freeSpot(i) then
            // Take the file
            val fileId = d(i)
            val fileIndex = i
            while (i < d.size && d(i) == fileId) do i += 1
            val fileSize = i - fileIndex
            res = res ++ List(File(fileId, fileIndex, fileSize))
        else
            i += 1
    }
    res
}
extension (d: Disk) def firstFreeSpaceOf(size: Int): Option[Int] = (0 until d.size - size).find(i => d.freeSpot(i, size))

extension (d: Disk) def compactWholeFiles(): Unit = {
    val files = d.asFiles.reverse
    // println(f"Files = $files")
    for (file <- files) do 
        // println(f"current Disk layout = ${d.toTextRepr()}")
        val freeSpot = d.firstFreeSpaceOf(file.size)
        freeSpot match
            case None => () //println(f"No free spot of size = ${file.size}")
            case Some(value) => {
                // println(f"Free spot of size = ${file.size} at $value")
                if value < file.index then 
                    d.moveFile(file, value)
            }
}

object Disk:
    def freeId: Int = -1
end Disk

object Day09:
    def parse(s: String): Disk = 
        var parsingFileBlock = true
        var fileId: Int = 0
        var res: List[Int] = Nil
        for (c <- s.toCharArray())  {
            val toAdd = if parsingFileBlock then {
                val id = fileId
                fileId += 1
                id
            } else Disk.freeId
            res = res ++ (0 until f"$c".toInt).map(_ => toAdd)
            parsingFileBlock = !parsingFileBlock

        }
        res.toArray
    def part01(d: Disk): Unit = d.compactDisk()
    def part02(d: Disk): Unit = d.compactWholeFiles()
end Day09

@main def Main09() =
    val inTestTest = "12345"
    val inTest = "2333133121414131402"
    // Part 1
    val diskTest = Day09.parse(inTest)
    val diskTestTest = Day09.parse(inTestTest)
    println(f"Test: Disk = ${diskTest.toTextRepr()}")    
    println(f"Test: Disk Test Test = ${diskTestTest.toTextRepr()}")    
    Day09.part01(diskTestTest)
    println(f"Test: Disk Test Test after compacting = ${diskTestTest.toTextRepr()}")
    println(f"Test: Disk Test Test checksum = ${diskTestTest.checksum}")
    Day09.part01(diskTest)
    // println(f"Test: Disk after compacting = ${diskTest.toTextRepr()}")
    println(f"Test: Disk checksum = ${diskTest.checksum}")


    val in = Utils09.openFile("09.txt").head
    val disk = Day09.parse(in)
    Day09.part01(disk)
    // println(f"Disk after compacting = ${disk.toTextRepr()}")
    println(f"Disk checksum = ${disk.checksum}")

    // Part 2
    val diskTest2 = Day09.parse(inTest)
    // println(f"Test: layout = ${diskTest2.toTextRepr()}")
    // println(f"Test: disk Files = ${diskTest2.asFiles}")
    Day09.part02(diskTest2)
    // println(f"Test: Disk after compacting whole files = ${diskTest2.toTextRepr()}")
    println(f"Test: Disk checksum = ${diskTest2.checksum}")

    val disk2 = Day09.parse(in)
    Day09.part02(disk2)
    // println(f"Disk after compacting whole files = ${disk2.toTextRepr()}")
    println(f"Disk checksum = ${disk2.checksum}")


        