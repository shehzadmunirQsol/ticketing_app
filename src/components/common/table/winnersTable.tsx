import React, { useState } from 'react';
import {
  ColumnDef,
  // ColumnFiltersState,
  SortingState,
  VisibilityState,
  flexRender,
  getCoreRowModel,
  getFilteredRowModel,
  getPaginationRowModel,
  getSortedRowModel,
  useReactTable,
} from '@tanstack/react-table';
import { ChevronDown } from 'lucide-react';

import { Button } from '@/ui/button';
import {
  DropdownMenu,
  DropdownMenuCheckboxItem,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from '@/ui/dropdown-menu';
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from '@/ui/table';
import { trpc } from '~/utils/trpc';
import { ScrollArea, ScrollBar } from '~/components/ui/scroll-area';
import { LoadingDialog } from '../modal/loadingModal';
import Image from 'next/image';
import { displayDate } from '~/utils/helper';

export type WinnerType = {
  Event: {
    id: number;
    EventDescription: {
      name: string;
    }[];
    thumb: string;
  };
  Customer: {
    email: string;
    id: number;
    first_name: string;
  };
  is_cash_alt: boolean;
  draw_date: Date | null;
  ticket_num: number;
};

export default function WinnersDataTable() {
  const [sorting, setSorting] = useState<SortingState>([]);
  const [filters, setFilters] = useState({
    first: 0,
    rows: 10,
    lang_id: 1,
  });
  const [columnVisibility, setColumnVisibility] = useState<VisibilityState>({});
  const [rowSelection, setRowSelection] = useState({});

  const { data, isLoading } = trpc.winner.get.useQuery(filters, {
    refetchOnWindowFocus: true,
  });

  const winnesData = React.useMemo(() => {
    return Array.isArray(data?.data) ? data?.data : [];
  }, [data]);

  const columns: ColumnDef<WinnerType>[] = [
    {
      accessorKey: 'Event',
      header: 'Event',
      cell: ({ row }) => {
        return (
          <div className="flex items-center gap-4 text-ellipsis whitespace-nowrap overflow-hidden">
            <Image
              className="object-cover bg-ac-2 h-10 w-16 rounded-lg"
              src={`${process.env.NEXT_PUBLIC_MEDIA_BASE_URL}${row?.original?.Event.thumb}`}
              alt={row?.original?.Event.EventDescription[0]?.name ?? ''}
              width={100}
              height={100}
            />

            <p className="w-40 text-ellipsis whitespace-nowrap overflow-hidden">
              {row?.original?.Event.EventDescription[0]?.name}
            </p>
          </div>
        );
      },
    },
    {
      accessorKey: 'Customer Name',
      header: 'Customer Name',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          {row?.original?.Customer?.first_name}
        </div>
      ),
    },
    {
      accessorKey: 'Customer Email',
      header: 'Customer Email',
      cell: ({ row }) => (
        <div className="text-ellipsis whitespace-nowrap ">
          {row?.original?.Customer?.email}
        </div>
      ),
    },
    {
      accessorKey: 'Draw Date',
      header: 'Draw Date',
      cell: ({ row }) => (
        <div className="text-ellipsis whitespace-nowrap">
          {displayDate(row?.original?.draw_date)}
        </div>
      ),
    },
    {
      accessorKey: 'Ticket number',
      header: 'Ticket number',
      cell: ({ row }) => (
        <div className="text-ellipsis whitespace-nowrap text-primary">
          #{row?.original?.ticket_num}
        </div>
      ),
    },
  ];
  const table = useReactTable({
    data: winnesData as WinnerType[],
    columns,
    onSortingChange: setSorting,
    getCoreRowModel: getCoreRowModel(),
    getPaginationRowModel: getPaginationRowModel(),
    getSortedRowModel: getSortedRowModel(),
    getFilteredRowModel: getFilteredRowModel(),
    onColumnVisibilityChange: setColumnVisibility,
    onRowSelectionChange: setRowSelection,
    state: {
      sorting,
      columnVisibility,
      rowSelection,
    },
  });

  function handlePagination(page: number) {
    if (page < 0) return;
    setFilters((prevFilters) => ({ ...prevFilters, first: page }));
  }

  return (
    <div className="w-full space-y-4">
      <div className="flex items-center justify-end gap-2">
        <DropdownMenu>
          <DropdownMenuTrigger asChild>
            <Button variant="outline">
              Columns <ChevronDown className="ml-2 h-4 w-4" />
            </Button>
          </DropdownMenuTrigger>
          <DropdownMenuContent align="end">
            {table
              .getAllColumns()
              .filter((column) => column.getCanHide())
              .map((column) => {
                return (
                  <DropdownMenuCheckboxItem
                    key={column.id}
                    className="capitalize"
                    checked={column.getIsVisible()}
                    onCheckedChange={(value) =>
                      column.toggleVisibility(!!value)
                    }
                  >
                    {column.id}
                  </DropdownMenuCheckboxItem>
                );
              })}
          </DropdownMenuContent>
        </DropdownMenu>
      </div>
      <div className="rounded-md border border-border">
        <ScrollArea className="w-full ">
          <ScrollBar orientation="horizontal"></ScrollBar>
          <Table>
            <TableHeader>
              {table?.getHeaderGroups().map((headerGroup) => (
                <TableRow key={headerGroup.id}>
                  {headerGroup.headers.map((header) => {
                    return (
                      <TableHead key={header.id}>
                        {header.isPlaceholder
                          ? null
                          : flexRender(
                              header.column.columnDef.header,
                              header.getContext(),
                            )}
                      </TableHead>
                    );
                  })}
                </TableRow>
              ))}
            </TableHeader>
            <TableBody>
              {table?.getRowModel()?.rows?.length ? (
                table?.getRowModel()?.rows?.map((row) => (
                  <TableRow
                    key={row.id}
                    data-state={row.getIsSelected() && 'selected'}
                  >
                    {row.getVisibleCells().map((cell) => (
                      <TableCell key={cell.id}>
                        {flexRender(
                          cell.column.columnDef.cell,
                          cell.getContext(),
                        )}
                      </TableCell>
                    ))}
                  </TableRow>
                ))
              ) : (
                <TableRow>
                  <TableCell
                    colSpan={columns.length}
                    className="h-24 text-center"
                  >
                    No results.
                  </TableCell>
                </TableRow>
              )}
            </TableBody>
          </Table>
        </ScrollArea>
      </div>
      <div className="flex items-center justify-end space-x-2 py-4">
        <div className="space-x-2">
          <Button
            variant="outline"
            onClick={() => handlePagination(filters.first - 1)}
            disabled={filters.first === 0}
          >
            Previous
          </Button>
          <Button
            variant="outline"
            onClick={() => handlePagination(filters.first + 1)}
            disabled={(filters.first + 1) * filters.rows > (data?.count || 0)}
          >
            Next
          </Button>
        </div>
      </div>

      <LoadingDialog open={isLoading} text={'Loading data...'} />
    </div>
  );
}
